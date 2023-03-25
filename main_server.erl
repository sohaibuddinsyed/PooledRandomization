-module(main_server).
-import(string,[substr/3]).
-import(string,[concat/2]).
-export([start/0,server_function/4,compute_sha_groups/3, start_actors/4, find_group/4, assign_groups/3]).

assign_groups(0, _, _) ->
  ok;

assign_groups(Number_Of_Members, Num_Miners, Group_Prefix_Sha_Values) ->
  Member_ID = base64:encode_to_string (crypto:strong_rand_bytes (rand:uniform (10))),
  spawn(main_server, start_actors, [Num_Miners, Group_Prefix_Sha_Values, Member_ID, 0]),
  assign_groups(Number_Of_Members-1, Num_Miners, Group_Prefix_Sha_Values).

find_group(_,_,0,_) ->
  ok;

find_group(Group_Prefix_Sha_Values, Member_ID, Num_Iterations, Random_Number) ->
  Random_String = concat (Member_ID, base64:encode_to_string (crypto:strong_rand_bytes (rand:uniform (5) + Random_Number))),
  Sha_value = lists:last(io_lib:format ("~64.16.0b", [binary:decode_unsigned (crypto:hash (sha256, Random_String))])),
  Prefix_value = substr(Sha_value, 1, 5),
  Match_found = orddict:is_key(Prefix_value, Group_Prefix_Sha_Values),
  if Match_found == true ->
    server ! {group_found, Member_ID, orddict:fetch(Prefix_value, Group_Prefix_Sha_Values)};
    true ->
      find_group(Group_Prefix_Sha_Values, Member_ID, Num_Iterations-1, Random_Number)
  end.


start_actors(0,_,_,_) ->
  ok;

start_actors(Num_Miners, Group_Prefix_Sha_Values, Member_ID, Random_Number) ->
  spawn(main_server, find_group, [Group_Prefix_Sha_Values, Member_ID, 50000, Random_Number]),
  start_actors(Num_Miners-1, Group_Prefix_Sha_Values, Member_ID, Random_Number+10).

compute_sha_groups(0, Group_Sha_Values, Group_Prefix_Sha_Values) ->
  {Group_Sha_Values, Group_Prefix_Sha_Values};

compute_sha_groups(Number_Of_Groups, Group_Sha_Values, Group_Prefix_Sha_Values) ->
  Random_String = base64:encode_to_string (crypto:strong_rand_bytes (rand:uniform (10))),
  Sha_value = lists:last(io_lib:format ("~64.16.0b", [binary:decode_unsigned (crypto:hash (sha256, Random_String))])),
  Valid_Sha_value = io_lib:char_list(Sha_value),
  if Valid_Sha_value == false ->
    compute_sha_groups(Number_Of_Groups, Group_Sha_Values, Group_Prefix_Sha_Values);
    true ->
      Sub_string = substr(Sha_value, 1, 5),
      compute_sha_groups(Number_Of_Groups-1, orddict:store(Sha_value, Random_String, Group_Sha_Values), orddict:store(Sub_string, Sha_value, Group_Prefix_Sha_Values))
  end.


server_function(Group_Sha_Values, Group_Prefix_Sha_Values, Members_Group_Info, Num_Miners)->
  receive
    %To create groups
    {create_groups, Number_Of_Groups} ->
      {Group_Sha_Values_,Group_Prefix_Sha_Values_}  = compute_sha_groups(Number_Of_Groups, [], []),
      io:format("Group IDs and Names ~n ~p ~n",[Group_Sha_Values_]),
      io:format("~n ~n Prefix and IDs ~n ~p ~n",[Group_Prefix_Sha_Values_]),
      io:format("Groups Created ~n"),
      server_function(Group_Sha_Values_, Group_Prefix_Sha_Values_, Members_Group_Info, Num_Miners);

    %To add additional groups to existing list
    {add_groups, Number_Of_Groups} ->
      {Group_Sha_Values_,Group_Prefix_Sha_Values_}  = compute_sha_groups(Number_Of_Groups, Group_Sha_Values, Group_Prefix_Sha_Values),
      io:format("Group IDs and Names ~n ~p ~n",[Group_Sha_Values_]),
      io:format("~n ~n Prefix and IDs ~n ~p ~n",[Group_Prefix_Sha_Values_]),
      io:format("Groups Added ~n"),
      server_function(Group_Sha_Values_, Group_Prefix_Sha_Values_, Members_Group_Info, Num_Miners);

    %Assign group to a given Member
    {assign_group, Member_ID} ->
      Check_if_group_assigned = orddict:is_key(Member_ID, Members_Group_Info),
      if Check_if_group_assigned ->
        Group_Name = orddict:fetch(Member_ID, Members_Group_Info),
        io:format("Member ~p already assigned to the group ~p ~n",[Member_ID,Group_Name]);
        true ->
        spawn(main_server, start_actors, [Num_Miners, Group_Prefix_Sha_Values, Member_ID, 0])
      end,
      server_function(Group_Sha_Values, Group_Prefix_Sha_Values, Members_Group_Info, Num_Miners);

    %Assign groups for a given number of members
    {assign_groups, Number_Of_Members} ->
      assign_groups(Number_Of_Members, Num_Miners, Group_Prefix_Sha_Values),
      server_function(Group_Sha_Values, Group_Prefix_Sha_Values, Members_Group_Info, Num_Miners);

    %Matching Group Found for a given Member ID
    {group_found, Member_ID, Group_ID} ->
      Check_if_group_assigned = orddict:is_key(Member_ID, Members_Group_Info),
      if Check_if_group_assigned ->
        server_function(Group_Sha_Values, Group_Prefix_Sha_Values, Members_Group_Info, Num_Miners);
        true ->
          Group_Name = orddict:fetch(Group_ID, Group_Sha_Values),
          io:format("Member ~p assigned to Group ~p ~n",[Member_ID, Group_Name]),
          server_function(Group_Sha_Values, Group_Prefix_Sha_Values, orddict:store(Member_ID, Group_Name, Members_Group_Info), Num_Miners)
      end;

    %Add additional Miners
    {add_miners, New_Miners} ->
      io:format("Miners added. New Miners count is ~p ~n",[Num_Miners+New_Miners]),
      server_function(Group_Sha_Values, Group_Prefix_Sha_Values, Members_Group_Info, Num_Miners+New_Miners)
  end.

start() ->
  % Registering server_function as server and initializing the values
  register(server, spawn(main_server, server_function, [[],[],[],5])).
