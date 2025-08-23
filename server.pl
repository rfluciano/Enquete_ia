:- use_module(library(http/http_parameters)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/json)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_dispatch)).

% Endpoint
:- http_handler(root(evaluate), evaluate_handler, []).

evaluate_handler(Request) :-
    ( current_predicate(http_cors_enable/2) ->
        http_cors_enable(Request, [methods([post,options]), origin('*')])
    ; true
    ),
    ( option(method(options), Request) ->
        reply_json_dict(_{ok:true})
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
                      ( evaluate(Suspect,Crime,Res) -> Reply = Res ; Reply = _{verdict:"error", message:"evaluation_failed"} ),
                      retract_terms(Terms)
                  ),
                  reply_json_dict(Reply)
                ),
                ErrEval,
                ( term_string(ErrEval, ErrString),   % ✅ conversion correcte
                  print_message(error, ErrEval),
                  reply_json_dict(_{error:true,message:ErrString}, [status(500)])
                )
            )
        ; reply_json_dict(_{error:true,message:"missing_fields"}, [status(400)])
        )
    ).


% --- Charger les faits existants ---
:- consult('facts.pl').

% --- Handlers ---
:- http_handler(root('api/questions'), questions_handler, [methods([post,options])]).
:- http_handler(root('api/evaluate'), evaluate_handler, [methods([post,options])]).
:- http_handler(root('api/suspect'), suspect_handler, [methods([get,options])]).

% --- Démarrage du serveur ---
start_server(Port) :-
    http_server(http_dispatch, [port(Port)]).

% --- Helper pour convertir string -> atom ---
normalize_arg(Val, Atom) :-
    ( atom(Val) -> Atom = Val
    ; string(Val) -> atom_string(Atom, Val)
    ; Atom = Val
    ).

% --- Ajout de faits via une requête POST ---
:- http_handler(root(add_fact), add_fact_handler, [methods([post,options])]).

add_fact_handler(Request) :-
    http_read_json_dict(Request, Dict),
    get_dict(suspect, Dict, Suspect0),
    get_dict(fact, Dict, Fact0),
    normalize_arg(Suspect0, Suspect),
    normalize_arg(Fact0, Fact),
    assertz(fait(Suspect, Fact)),          % ou l’équivalent de ta base
    reply_json_dict(_{ok:true, message:"Fait ajouté"}).

% --- Mise à jour de faits via une requête POST ---
:- http_handler(root(update_fact), update_fact_handler, [methods([post,options])]).

update_fact_handler(Request) :-
    http_read_json_dict(Request, Dict),
    get_dict(suspect, Dict, Suspect0),
    get_dict(old, Dict, Old0),
    get_dict(new, Dict, New0),
    normalize_arg(Suspect0, Suspect),
    normalize_arg(Old0, OldFact),
    normalize_arg(New0, NewFact),
    ( update_fact(Suspect, OldFact, NewFact) ->
        reply_json_dict(_{ok:true, message:"Fait mis à jour"})
    ; reply_json_dict(_{ok:false, message:"Aucun fait trouvé"})
    ).


% --- Suppression de faits via une requête POST ---
delete_fact_handler(Request) :-
    http_read_json_dict(Request, Dict),
    get_dict(suspect, Dict, Suspect0),
    get_dict(fact, Dict, Fact0),
    normalize_arg(Suspect0, Suspect),
    normalize_arg(Fact0, Fact),
    retractall(fait(Suspect, Fact)),
    reply_json_dict(_{ok:true, message:"Fait supprimé"}).

% --- QUESTIONS HANDLER ---
questions_handler(Request) :-
    ( current_predicate(http_cors_enable/2) ->
        http_cors_enable(Request, [methods([post,options]), origin('*')])
    ; true
    ),
    ( option(method(options), Request) ->
        reply_json_dict(_{ok:true})
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

% --- Définition des questions ---
question_for(S, Crime, _{id:alibi, text:Text}) :-
    format(atom(Text),'~w a-t-il/elle un alibi pour ~w ?', [S,Crime]),
    \+ has_alibi(S,Crime).

question_for(S, Crime, _{id:motif, text:Text}) :-
    (Crime == vol ; Crime == assassinat),
    format(atom(Text),'~w a-t-il/elle un motif pour ~w ?', [S,Crime]),
    \+ has_motive(S,Crime).

question_for(S, Crime, _{id:proximite, text:Text}) :-
    (Crime == vol ; Crime == assassinat),
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

% --- EVALUATE HANDLER ---
evaluate_handler(Request) :-
    ( current_predicate(http_cors_enable/2) ->
        http_cors_enable(Request, [methods([post,options]), origin('*')])
    ; true
    ),
    ( option(method(options), Request) ->
        reply_json_dict(_{ok:true})
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
                      ( evaluate(Suspect,Crime,Res) -> Reply = Res ; Reply = _{verdict:"error", message:"evaluation_failed"} ),
                      retract_terms(Terms)
                  ),
                  reply_json_dict(Reply)
                ),
                ErrEval,
                ( print_message(error, ErrEval),
                  reply_json_dict(_{error:true,message:to_string(ErrEval)}, [status(500)])
                )
            )
        ; reply_json_dict(_{error:true,message:"missing_fields"}, [status(400)])
        )
    ).

answers_to_terms(Dict, S, C, Terms) :-
    dict_pairs(Dict, _, Pairs),
    maplist(pair_to_term(S,C), Pairs, Terms0),
    exclude(=(none), Terms0, Terms).

pair_to_term(_S,_C, id-_, none) :- !.
pair_to_term(S, C, motif-true, has_motive(S,C)) :- !.
pair_to_term(_S,_C, motif-false, none) :- !.
pair_to_term(S, C, alibi-true, has_alibi(S,C)) :- !.
pair_to_term(_S,_C, alibi-false, none) :- !.
pair_to_term(S, C, proximite-true, was_near_crime_scene(S,C)) :- !.
pair_to_term(S, C, empreintes-true, has_fingerprint_on_weapon(S,C)) :- !.
pair_to_term(S, C, preuve-true, has_fingerprint_on_weapon(S,C)) :- !.
pair_to_term(_S,_C, preuve-false, none) :- !.
pair_to_term(S, C, transaction-true, has_bank_transaction(S,C)) :- !.
pair_to_term(S, C, fausse_id-true, owns_fake_identity(S,C)) :- !.
pair_to_term(_,_,_,none).

assert_terms([]).
assert_terms([T|Ts]) :- assertz(T), assert_terms(Ts).

retract_terms([]).
retract_terms([T|Ts]) :- retractall(T), retract_terms(Ts).

evaluate(S, C, _{verdict:Verdict, supporting:Supp, missing:Miss}) :-
    ( has_alibi(S,C) -> Verdict = "not_guilty (alibi)"
    ; ( is_guilty(S,C) -> Verdict = "guilty" ; Verdict = "not_guilty (preuves insuffisantes)" )
    ),
    ( supporting_facts(S,C,Sf) -> true ; Sf = [] ),
    ( missing_evidence(S,C,MissL) -> true ; MissL = [] ),
    maplist(term_string, Sf, Supp),
    maplist(term_string, MissL, Miss).


% --- SUSPECT HANDLER ---
suspect_handler(Request) :-
    ( current_predicate(http_cors_enable/2) ->
        http_cors_enable(Request, [methods([get,options]), origin('*')])
    ; true
    ),
    ( option(method(options), Request) ->
        reply_json_dict(_{ok:true})
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 🔥 AJOUT MANQUANT : règles d’évaluation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Définir quand un suspect est coupable
is_guilty(S, C) :-
    has_motive(S,C),
    was_near_crime_scene(S,C).

% Faits à l'appui
supporting_facts(S,C,[has_motive(S,C)]) :-
    has_motive(S,C), !.
supporting_facts(S,C,[was_near_crime_scene(S,C)]) :-
    was_near_crime_scene(S,C), !.
supporting_facts(_,_,[]).

% Preuves manquantes
missing_evidence(S,C,[has_motive(S,C)]) :-
    \+ has_motive(S,C), !.
missing_evidence(S,C,[was_near_crime_scene(S,C)]) :-
    \+ was_near_crime_scene(S,C), !.
missing_evidence(_,_,[]).
