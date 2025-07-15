%% @author Driebit
%% @copyright 2025 Driebit

-module(mod_driebit_activity).
-author("Driebit <tech@driebit.nl>").

-mod_title("Driebit Activity module").
-mod_description("Tracking and managing user activities").

% Note: this was picked so that the priority is higher than mod_driebit_rdf's
-mod_prio(450).
-mod_schema(1).
-mod_provides([activity]).
-mod_depends([rdf]).

-export([
    manage_schema/2,
    manage_data/2
]).

manage_schema(_Version, _Context) ->
    ok.
manage_data(_Version, _Context) ->
    ok.
