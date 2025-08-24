:- dynamic suspect/1.
:- dynamic has_alibi/2.
:- dynamic has_motive/2.
:- dynamic was_near_crime_scene/2.
:- dynamic has_fingerprint_on_weapon/2.
:- dynamic eyewitness_identification/2.
:- dynamic owns_fake_identity/2.
:- dynamic has_bank_transaction/2.

% --- Suspects connus ---
suspect(john).
suspect(mary).
suspect(alice).
suspect(elvis).

% --- Exemples de faits initiaux ---
has_alibi(mary, assassinat).
has_alibi(alice, escroquerie).

has_motive(mary, vol).
has_motive(mary, assassinat).

was_near_crime_scene(john, vol).
was_near_crime_scene(mary, assassinat).

has_fingerprint_on_weapon(john, vol).
has_fingerprint_on_weapon(alice, assassinat).

eyewitness_identification(john, assassinat).
owns_fake_identity(alice, escroquerie).
has_bank_transaction(alice, escroquerie).
