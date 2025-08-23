:- use_module(library(readutil)).   % read_line_to_string/2
:- use_module('./db.pl').
:- use_module('./inference.pl').

% normalisation
normalize_answer(Ans, yes) :-
    string_lower(Ans, L), sub_string(L,0,1,_,C), memberchk(C, ["o","y","1"]).
normalize_answer(Ans, no) :-
    string_lower(Ans, L), sub_string(L,0,1,_,C), memberchk(C, ["n","0"]).

ask_open(Prompt, Ans) :-
    format('~w : ', [Prompt]),
    read_line_to_string(user_input, Ans).

ask_yesno_fuzzy(Prompt, Fact, Source) :-
    format('~w (o/n) : ', [Prompt]),
    read_line_to_string(user_input, R0),
    ( normalize_answer(R0, yes) ->
        ( nonvar(Fact) -> record_fact(Fact, Source) ; true ),
        record_history(asked(Prompt)-yes)
    ; normalize_answer(R0, no) ->
        ask_open('Pourquoi ?', Why),
        ( nonvar(Fact) -> assertz(reason(Fact, Why)), assertz(dialog_line(Prompt-no(Why))) ; true ),
        record_history(asked(Prompt)-no(Why))
    ; writeln('Reponse non comprise, reessayer.'), ask_yesno_fuzzy(Prompt, Fact, Source)
    ).

ask_choice(Prompt, Options, Choice) :-
    format('~w~n', [Prompt]),
    list_choices(Options,1),
    format('Numero : '),
    read_line_to_string(user_input, R0),
    catch(number_string(N,R0),_,fail),
    nth1(N, Options, Choice), !.
ask_choice(_,_,_) :- writeln('Choix invalide.'), fail.

list_choices([], _).
list_choices([H|T], N) :-
    format('  ~d) ~w~n', [N,H]),
    N2 is N+1, list_choices(T, N2).

explain_with_sources(S, C) :-
    supporting_facts(S, C, Facts),
    format('Preuves: ~w~n', [Facts]),
    missing_evidence(S, C, Miss),
    format('Manquantes: ~w~n', [Miss]).

% poser toutes les questions necessaires
ensure_all_questions(S, Crime) :-
    ( has_alibi(S, Crime) -> true
    ; format(atom(QA),'~w a-t-il/elle un alibi pour ~w ?', [S, Crime]),
      ask_yesno_fuzzy(QA, has_alibi(S, Crime), user)
    ),
    ( Crime = vol ->
        format(atom(QM),'~w a-t-il/elle un motif pour vol ?', [S]),
        ask_yesno_fuzzy(QM, has_motive(S, vol), user),
        format(atom(QP),'~w etait-il/elle pres du lieu du vol ?', [S]),
        ask_yesno_fuzzy(QP, was_near_crime_scene(S, vol), user),
        format(atom(QF),'empreintes de ~w sur l\'arme du vol ?', [S]),
        ask_yesno_fuzzy(QF, has_fingerprint_on_weapon(S, vol), user)
    ; Crime = assassinat ->
        format(atom(QM2),'~w a-t-il/elle un motif pour assassinat ?', [S]),
        ask_yesno_fuzzy(QM2, has_motive(S, assassinat), user),
        format(atom(QP2),'~w etait-il/elle pres du lieu de l\'assassinat ?', [S]),
        ask_yesno_fuzzy(QP2, was_near_crime_scene(S, assassinat), user),
        ( ask_choice('preuve ?', ['empreinte','temoin','aucun'], Choice) ->
            ( Choice = 'empreinte' -> record_fact(has_fingerprint_on_weapon(S, assassinat), user)
            ; Choice = 'temoin' -> record_fact(eyewitness_identification(S, assassinat), user)
            ; true)
        ; true )
    ; Crime = escroquerie ->
        format(atom(QB),'transaction bancaire frauduleuse de ~w ?', [S]),
        ask_yesno_fuzzy(QB, has_bank_transaction(S, escroquerie), user),
        format(atom(QI),'~w possede-t-il/elle une fausse identite ?', [S]),
        ask_yesno_fuzzy(QI, owns_fake_identity(S, escroquerie), user)
    ; true ).

% entree en langage naturel simple
ask_nl :-
    format('Question (ex: "est-ce que mary est coupable pour assassinat")~n> '),
    read_line_to_string(user_input, Input),
    parse_nl(Input, Suspect, Crime),
    format('Interrogation pour ~w / ~w~n', [Suspect, Crime]),
    ensure_all_questions(Suspect, Crime),
    ( has_alibi(Suspect, Crime) ->
        format('Conclusion: not_guilty (alibi connu).~n'), explain_with_sources(Suspect, Crime)
    ; ( is_guilty(Suspect, Crime) ->
            format('Conclusion: guilty.~n'), explain_with_sources(Suspect, Crime)
      ; format('Conclusion: not_guilty (preuves insuffisantes).~n'), explain_with_sources(Suspect, Crime)
      )
    ).

% minimal parse helper (cherche suspect et type)
parse_nl(Input, Suspect, Crime) :-
    string_lower(Input, L),
    findall(S, suspect(S), Ss),
    maplist(atom_string, Ss, SStrs),
    include({L}/[X]>>sub_string(L, _, _, _, X), SStrs, Matches),
    ( Matches = [Sname|_] -> atom_string(Suspect, Sname)
    ; format('Quel suspect (choisir parmi: ~w) ?~n', [Ss]),
      list_choices(Ss,1),
      read_line_to_string(user_input,R0),
      catch(number_string(N,R0),_,fail), nth1(N,Ss,Suspect)
    ),
    ( (sub_string(L,_,_,_,"vol") -> Crime=vol
      ; sub_string(L,_,_,_,"escroquerie") -> Crime=escroquerie
      ; sub_string(L,_,_,_,"assassin") -> Crime=assassinat
      ) -> true
    ; Crimes=[vol,assassinat,escroquerie],
      format('Quel crime ?~n'), list_choices(Crimes,1),
      read_line_to_string(user_input,R1), catch(number_string(N2,R1),_,fail), nth1(N2,Crimes,Crime)
    ).