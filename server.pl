/* =======================
   server.pl (version 8 + facts_state corrigé)
   ======================= */

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/json)).

% Charger connaissances + regles d'inference
:- [facts].
:- [inference].

% ---------------- CORS global (optionnel) ----------------
:- if(current_predicate(http:cors_enable/0)).
:- set_setting(http:cors, [*]).
:- endif.

% ---------------- Routes ----------------
:- http_handler(root('api/questions'), questions_handler, [methods([post,options])]).
:- http_handler(root('api/evaluate'),  evaluate_handler,  [methods([post,options])]).
:- http_handler(root('api/suspect'),   suspect_handler,   [methods([get,options])]).
:- http_handler(root(add_fact),        add_fact_handler,  [methods([post,options])]).
:- http_handler(root(update_fact),     update_fact_handler,[methods([post,options])]).
:- http_handler(root(delete_fact),     delete_fact_handler,[methods([post,options])]).
:- http_handler(root('api/facts_state'), facts_state_handler, [methods([get,options])]).

% ---------------- Demarrage ----------------
start_server(Port) :- http_server(http_dispatch, [port(Port)]).

% ---------------- Utilitaires ----------------
normalize_arg(Val, Atom) :-
    ( atom(Val)   -> Atom = Val
    ; string(Val) -> atom_string(Atom, Val)
    ;                Atom = Val
    ).

answers_to_terms(Dict, S, C, Terms) :-
    dict_pairs(Dict, _, Pairs),
    maplist(pair_to_term(S,C), Pairs, Terms0),
    exclude(=(none), Terms0, Terms).

pair_to_term(_S,_C, id-_, none) :- !.
pair_to_term(S, C, motif-true,                has_motive(S,C)) :- !.
pair_to_term(_S,_C, motif-false,              none) :- !.
pair_to_term(S, C, alibi-true,                has_alibi(S,C)) :- !.
pair_to_term(_S,_C, alibi-false,              none) :- !.
pair_to_term(S, C, proximite-true,            was_near_crime_scene(S,C)) :- !.
pair_to_term(S, C, empreintes-true,           has_fingerprint_on_weapon(S,C)) :- !.
pair_to_term(S, C, preuve-true,               has_fingerprint_on_weapon(S,C)) :- !.
pair_to_term(_S,_C, preuve-false,             none) :- !.
pair_to_term(S, C, transaction-true,          has_bank_transaction(S,C)) :- !.
pair_to_term(S, C, fausse_id-true,            owns_fake_identity(S,C)) :- !.
pair_to_term(_,_,_, none).

assert_terms([]).
assert_terms([T|Ts]) :- assertz(T), assert_terms(Ts).

retract_terms([]).
retract_terms([T|Ts]) :- retractall(T), retract_terms(Ts).

% ---------------- QUESTIONS ----------------
questions_handler(Request) :-
    ( current_predicate(http_cors_enable/2) ->
        http_cors_enable(Request, [methods([post,options]), origin('*')])
    ; true ),
    ( option(method(options), Request) -> reply_json_dict(_{ok:true})
    ;
        http_read_json_dict(Request, Dict),
        ( get_dict(suspect, Dict, Sus0) -> true ; Sus0 = _ ),
        ( get_dict(crime,   Dict, Crime0) -> true ; Crime0 = _ ),
        normalize_arg(Sus0, Suspect),
        normalize_arg(Crime0, Crime),
        Answers = Dict.get(answers, _{}),
        answers_to_terms(Answers, Suspect, Crime, Terms),
        setup_call_cleanup(
            assert_terms(Terms),
            find_questions(Suspect, Crime, Questions),
            retract_terms(Terms)
        ),
        reply_json_dict(_{questions:Questions})
    ).

find_questions(S, Crime, Qs) :-
    findall(Q, question_for(S, Crime, Q), Qs).

% Generation des questions filtrées
question_for(S, Crime, _{id:alibi, text:Text}) :-
    format(atom(Text),'~w a-t-il/elle un alibi pour ~w ?', [S,Crime]),
    \+ has_alibi(S,Crime).

question_for(S, Crime, _{id:motif, text:Text}) :-
    member(Crime,[vol,assassinat]),
    format(atom(Text),'~w a-t-il/elle un motif pour ~w ?', [S,Crime]),
    \+ has_motive(S,Crime).

question_for(S, Crime, _{id:proximite, text:Text}) :-
    member(Crime,[vol,assassinat]),
    format(atom(Text),'~w etait-il/elle pres du lieu du ~w ?', [S,Crime]),
    \+ was_near_crime_scene(S,Crime).

question_for(S, Crime, _{id:empreintes, text:Text}) :-
    Crime == vol,
    format(atom(Text),'empreintes de ~w sur l\'arme du vol ?', [S]),
    \+ has_fingerprint_on_weapon(S,vol).

question_for(S, Crime, _{id:preuve, text:Text}) :-
    Crime == assassinat,
    format(atom(Text),'empreintes ou temoin identifiant ~w ?', [S]),
    \+ has_fingerprint_on_weapon(S,assassinat),
    \+ eyewitness_identification(S,assassinat).

question_for(S, Crime, _{id:transaction, text:Text}) :-
    Crime == escroquerie,
    format(atom(Text),'transaction bancaire frauduleuse de ~w ?', [S]),
    \+ has_bank_transaction(S,escroquerie).

question_for(S, Crime, _{id:fausse_id, text:Text}) :-
    Crime == escroquerie,
    format(atom(Text),'~w possede-t-il/elle une fausse identite ?', [S]),
    \+ owns_fake_identity(S,escroquerie).

% ---------------- EVALUATE ----------------
evaluate_handler(Request) :-
    ( current_predicate(http_cors_enable/2) ->
        http_cors_enable(Request, [methods([post,options]), origin('*')])
    ; true ),
    ( option(method(options), Request) -> reply_json_dict(_{ok:true})
    ;
        catch(http_read_json_dict(Request, Dict), ErrRead,
              ( print_message(error, ErrRead),
                reply_json_dict(_{error:true,message:"invalid_json"}, [status(400)]), !, fail )),
        ( get_dict(suspect, Dict, Sus0), get_dict(crime, Dict, Crime0) ->
            normalize_arg(Sus0, Suspect),
            normalize_arg(Crime0, Crime),
            Answers = Dict.get(answers, _{}),
            catch(
                ( answers_to_terms(Answers, Suspect, Crime, Terms),
                  setup_call_cleanup(
                      assert_terms(Terms),
                      evaluate_and_reply(Suspect, Crime, Reply),
                      retract_terms(Terms)
                  ),
                  reply_json_dict(Reply)
                ),
                ErrEval,
                ( term_string(ErrEval, ErrString),
                  print_message(error, ErrEval),
                  reply_json_dict(_{error:true,message:ErrString}, [status(500)])
                )
            )
        ; reply_json_dict(_{error:true,message:"missing_fields"}, [status(400)])
        )
    ).

evaluate_and_reply(S, Crime, Reply) :-
    verdict(S, Crime, V0),
    Preuves0 = V0.preuves,
    maplist(term_string, Preuves0, PreuvesStr),
    ( supporting_facts(S, Crime, Humaines) -> true ; Humaines = [] ),
    Reply = V0.put(_{ preuves: PreuvesStr, preuves_humaines: Humaines }).

% ---------------- SUSPECT ----------------
suspect_handler(Request) :-
    ( current_predicate(http_cors_enable/2) ->
        http_cors_enable(Request, [methods([get,options]), origin('*')])
    ; true ),
    ( option(method(options), Request) -> reply_json_dict(_{ok:true})
    ;
        http_parameters(Request, [id(Suspect,[atom])]),
        collect_suspect_facts(Suspect, Facts),
        reply_json_dict(Facts)
    ).

collect_suspect_facts(Suspect, Facts) :-
    findall(C, has_alibi(Suspect, C), Alibis),
    findall(C, has_motive(Suspect, C), Motives),
    findall(C, was_near_crime_scene(Suspect, C), NearScenes),
    findall(C, has_fingerprint_on_weapon(Suspect, C), Prints),
    findall(C, eyewitness_identification(Suspect, C), Witnessed),
    findall(C, owns_fake_identity(Suspect, C), FakeIds),
    findall(C, has_bank_transaction(Suspect, C), Transactions),
    maplist(atom_string, Alibis, AlibisStr),
    maplist(atom_string, Motives, MotivesStr),
    maplist(atom_string, NearScenes, NearScenesStr),
    maplist(atom_string, Prints, PrintsStr),
    maplist(atom_string, Witnessed, WitnessedStr),
    maplist(atom_string, FakeIds, FakeIdsStr),
    maplist(atom_string, Transactions, TransactionsStr),
    Facts = _{
        suspect: Suspect,
        alibis: AlibisStr,
        motives: MotivesStr,
        near_crime_scene: NearScenesStr,
        fingerprints: PrintsStr,
        eyewitness: WitnessedStr,
        fake_identities: FakeIdsStr,
        transactions: TransactionsStr
    }.

% ---------------- CRUD ----------------
add_fact_handler(Request) :-
    http_read_json_dict(Request, Dict),
    get_dict(suspect, Dict, Suspect0),
    get_dict(fact, Dict, Fact0),
    normalize_arg(Suspect0, S),
    normalize_arg(Fact0, FAtom),
    ( FAtom =.. [Pred, Crime] ->
        Term =.. [Pred, S, Crime],
        assertz(Term),
        reply_json_dict(_{ok:true, message:"Fait ajoute", term:Term})
    ; reply_json_dict(_{ok:false, message:"Format de 'fact' invalide"})
    ).

update_fact_handler(_Request) :-
    reply_json_dict(_{ok:false, message:"update_fact non implemente"}).

delete_fact_handler(Request) :-
    http_read_json_dict(Request, Dict),
    get_dict(suspect, Dict, Suspect0),
    get_dict(fact, Dict, Fact0),
    normalize_arg(Suspect0, S),
    normalize_arg(Fact0, FAtom),
    ( FAtom =.. [Pred, Crime] ->
        Term =.. [Pred, S, Crime],
        retractall(Term),
        reply_json_dict(_{ok:true, message:"Fait supprime", term:Term})
    ; reply_json_dict(_{ok:false, message:"Format de 'fact' invalide"})
    ).

% ---------------- FACTS STATE (strict) ----------------
facts_state_handler(Request) :-
    ( current_predicate(http_cors_enable/2) ->
        http_cors_enable(Request, [methods([get,options]), origin('*')])
    ; true ),
    ( option(method(options), Request) -> reply_json_dict(_{ok:true})
    ;
        http_parameters(Request, [suspect(Suspect0,[atom]), crime(Crime0,[atom])]),
        normalize_arg(Suspect0, S),
        normalize_arg(Crime0, Crime),
        facts_state(S, Crime, List),
        reply_json_dict(_{facts_state:List})
    ).

facts_state(S, Crime, FactsList) :-
    findall(Fact, (
        fact_for_crime(S, Crime, Fact)
    ), FactsList).

% ---------------- Construction des facts existants ----------------
fact_for_crime(S, Crime, _{id:alibi, text:Text, value:true}) :-
    has_alibi(S, Crime),
    format(atom(Text),'~w a-t-il/elle un alibi pour ~w ?', [S, Crime]).

fact_for_crime(S, Crime, _{id:motif, text:Text, value:true}) :-
    has_motive(S, Crime),
    format(atom(Text),'~w a-t-il/elle un motif pour ~w ?', [S, Crime]).

fact_for_crime(S, Crime, _{id:proximite, text:Text, value:true}) :-
    was_near_crime_scene(S, Crime),
    format(atom(Text),'~w etait-il/elle pres du lieu du ~w ?', [S, Crime]).

fact_for_crime(S, vol, _{id:empreintes, text:Text, value:true}) :-
    has_fingerprint_on_weapon(S, vol),
    format(atom(Text),'empreintes de ~w sur l\'arme du vol ?', [S]).

fact_for_crime(S, assassinat, _{id:preuve, text:Text, value:true}) :-
    (has_fingerprint_on_weapon(S, assassinat) ; eyewitness_identification(S, assassinat)),
    format(atom(Text),'empreintes ou temoin identifiant ~w ?', [S]).

fact_for_crime(S, escroquerie, _{id:transaction, text:Text, value:true}) :-
    has_bank_transaction(S, escroquerie),
    format(atom(Text),'transaction bancaire frauduleuse de ~w ?', [S]).

fact_for_crime(S, escroquerie, _{id:fausse_id, text:Text, value:true}) :-
    owns_fake_identity(S, escroquerie),
    format(atom(Text),'~w possede-t-il/elle une fausse identite ?', [S]).

update_fact(S, Crime, Fact0, Fact) :-
    Fact0 = _{id:ID, crime:_CrimeType, text:_, value:_},
    text_for(ID, S, Crime, Text),
    value_for(ID, S, Crime, V),
    Fact = Fact0.put(_{text:Text, value:V}).

% Texte pour chaque type de fait
text_for(alibi, S, Crime, Text) :-
    format(atom(Text), '~w a-t-il/elle un alibi pour ~w ?', [S, Crime]).

text_for(motif, S, Crime, Text) :-
    (Crime = vol ; Crime = assassinat),
    format(atom(Text), '~w a-t-il/elle un motif pour ~w ?', [S, Crime]).
text_for(motif, _, Crime, '') :-
    Crime \= vol,
    Crime \= assassinat.

text_for(proximite, S, Crime, Text) :-
    (Crime = vol ; Crime = assassinat),
    format(atom(Text), '~w etait-il/elle pres du lieu du ~w ?', [S, Crime]).
text_for(proximite, _, Crime, '') :-
    Crime \= vol,
    Crime \= assassinat.

text_for(empreintes, S, vol, Text) :-
    format(atom(Text), 'empreintes de ~w sur l\'arme du vol ?', [S]).
text_for(empreintes, _, Crime, '') :-
    Crime \= vol.

text_for(preuve, S, assassinat, Text) :-
    format(atom(Text), 'empreintes ou temoin identifiant ~w ?', [S]).
text_for(preuve, _, Crime, '') :-
    Crime \= assassinat.

text_for(transaction, S, escroquerie, Text) :-
    format(atom(Text), 'transaction bancaire frauduleuse de ~w ?', [S]).
text_for(transaction, _, Crime, '') :-
    Crime \= escroquerie.

text_for(fausse_id, S, escroquerie, Text) :-
    format(atom(Text), '~w possede-t-il/elle une fausse identite ?', [S]).
text_for(fausse_id, _, Crime, '') :-
    Crime \= escroquerie.


% Valeur de chaque fait
value_for(alibi, S, Crime, V) :-
    (has_alibi(S, Crime) -> V=true ; V=false).

value_for(motif, S, Crime, V) :-
    (has_motive(S, Crime) -> V=true ; V=false).

value_for(proximite, S, Crime, V) :-
    (was_near_crime_scene(S, Crime) -> V=true ; V=false).

value_for(empreintes, S, vol, V) :-
    (has_fingerprint_on_weapon(S, vol) -> V=true ; V=false).
value_for(empreintes, _, Crime, false) :-
    Crime \= vol.

value_for(preuve, S, assassinat, V) :-
    ((has_fingerprint_on_weapon(S, assassinat) ; eyewitness_identification(S, assassinat)) -> V=true ; V=false).
value_for(preuve, _, Crime, false) :-
    Crime \= assassinat.

value_for(transaction, S, escroquerie, V) :-
    (has_bank_transaction(S, escroquerie) -> V=true ; V=false).
value_for(transaction, _, Crime, false) :-
    Crime \= escroquerie.

value_for(fausse_id, S, escroquerie, V) :-
    (owns_fake_identity(S, escroquerie) -> V=true ; V=false).
value_for(fausse_id, _, Crime, false) :-
    Crime \= escroquerie.


% Filtrage
relevant_for(Crime, Fact) :-
    C = Fact.crime,
    ( C = all
    ; C = vol_assassinat, (Crime = vol ; Crime = assassinat)
    ; C = Crime
    ).
