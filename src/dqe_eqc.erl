-module(dqe_eqc).

-ifdef(HAS_EQC).

-include_lib("eqc/include/eqc.hrl").

-export([select_stmt/0]).

str() ->
    list(choose($a, $z)).

non_empty_string() ->
    ?SUCHTHAT(L, str(), length(L) >= 2).

non_empty_binary() ->
    ?LET(L, non_empty_string(), list_to_binary(L)).

pos_int() ->
    ?SUCHTHAT(N, int(), N > 0).

pos_real() ->
    ?SUCHTHAT(R, real(), R > 0).

time_unit() ->
    oneof([ms, s, m, h, d, w]).

time_type() ->
    #{op => time,
      return => time,
      signature => [integer,time_unit],
      args => [pos_int(), time_unit()]}.

aggr_range() ->
    time_type().

non_empty_list(T) ->
    ?SUCHTHAT(L, list(T), L /= []).

rel_time() ->
    oneof([
           pos_int(),
           now,
           #{op => ago, args => [time_type()]}
          ]).

aliases() ->
    [].

select_stmt() ->
    {select,
     non_empty_list(?SIZED(Size, maybe_named(Size))),
     aliases(),
     oneof([
            #{op => last, args => [pos_int()]},
            #{op => between, args => [rel_time(), pos_int()]},
            #{op => before, args => [rel_time(), pos_int()]},
            #{op => 'after', args => [pos_int(), pos_int()]}
           ])}.

aggr_tree(Size) ->
    ?LETSHRINK(
       [Q], [qry_tree(Size - 1)],
       oneof(
         [
          aggr1(Q),
          aggr2(Q),
          comb(Q),
          arith(Q)
         ])).

arith_fun() ->
    oneof([<<"add">>, <<"divide">>, <<"mul">>, <<"sub">>]).
arith_const() ->
    oneof([pos_int(), pos_real()]).
arith(Q) ->
    #{op => fcall,
      args => #{name => arith_fun(),
                inputs => [Q, arith_const()]}}.

aggr2_fun() ->
    oneof([<<"avg">>, <<"sum">>, <<"min">>, <<"max">>]).
aggr2(Q) ->
    #{op => fcall,
      args => #{name => aggr2_fun(),
                inputs => [Q, aggr_range()]}}.
aggr1_fun() ->
    oneof([<<"derivate">>]).
aggr1(Q) ->
    #{op => fcall,
      args => #{name => aggr1_fun(),
                inputs => [Q]}}.
comb_fun() ->
    oneof([<<"sum">>]).
comb(Q) ->
    #{op => fcall,
      args => #{name => comb_fun(),
                inputs => [Q]}}.

get_f() ->
    #{
      op        => get,
      args      => bm(),
      signature => [integer, integer, integer, metric, bucket],
      return    => metric
    }.

sget_f() ->
    #{
      op        => sget,
      args      => glob_bm(),
      signature => [integer, integer, integer, glob, bucket],
      return    => metric
    }.

pvar() ->
    {pvar, choose(1, 1)}.

named_element() ->
    oneof([
           non_empty_binary(),
           %%dvar(),
           pvar()
           ]).
named() ->
    ?SUCHTHAT(L, list(named_element()), L =/= []).

maybe_named(S) ->
    oneof([
           qry_tree(S),
           #{
              op   => named,
              args => [named(), qry_tree(S)],
              return => undefined
            }
          ]).

qry_tree(S) when S < 1->
    oneof([
           get_f(),
           sget_f(),
           lookup()
          ]);

qry_tree(Size) ->
    ?LAZY(frequency(
            [
             %%{1, comb_tree(Size)},
             %%{1, hist_tree(Size)},
             {10, aggr_tree(Size)}]
           )).

bucket() ->
    non_empty_binary().

metric() ->
    non_empty_list(non_empty_binary()).

lqry_metric() ->
    oneof([metric(), undefined]).

glob_metric() ->
    ?SUCHTHAT(L,
              non_empty_list(oneof([non_empty_binary(),'*'])),
              lists:member('*', L)).

glob_bm() ->
    [bucket(), glob_metric()].

bm() ->
    [bucket(), metric()].

lookup() ->
    #{op   => lookup,
      return => metric,
      args => ?SIZED(S, lookup(S))}.

lookup(0) ->
    [bucket(), lqry_metric()];
lookup(S) ->
    [bucket(), lqry_metric(), where_clause(S)].

tag() ->
    frequency(
      [{10, {tag, non_empty_binary(), non_empty_binary()}},
       {1,  {tag, <<>>, non_empty_binary()}}]).

where_clause(S) when S =< 1 ->
    oneof([{'=', tag(), non_empty_binary()},
           {'!=', tag(), non_empty_binary()}]);
where_clause(S) ->
    ?LAZY(?LET(N, choose(0, S - 1), where_clause_choice(N, S))).

where_clause_choice(N, S) ->
    oneof([{'and', where_clause(N), where_clause(S - N)}
           %%,{'or', where_clause(N), where_clause(S - N)}
          ]).

-endif.
