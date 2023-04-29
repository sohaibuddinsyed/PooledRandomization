-module(main_server).
-import(string,[substr/3]).
-import(string,[concat/2]).
-export([start/0,server_function/5,compute_sha_groups/4, start_actors/5, find_group/5, assign_groups/4, verify_nonce/4, start_verify_actors/5, verification/1, compute_xor/2, find_winner_group/3]).

assign_groups(0, _, _, _) ->
  ok;

%Function to assign groups for a given set of Members
assign_groups(Number_Of_Members, Num_Miners, Group_Prefix_Sha_Values, Curr_Member) ->
  %Computes random Member ID
  Member = concat("Member_",integer_to_list(Curr_Member)),
  Member_Prefix = concat(Member,"_"),
  Member_ID = concat(Member_Prefix,base64:encode_to_string (crypto:strong_rand_bytes (rand:uniform (10)))),

  %Spawns actors to find group by performing proof of work
  spawn(main_server, start_actors, [Num_Miners, Group_Prefix_Sha_Values, Member_ID, 0, 1]),
  assign_groups(Number_Of_Members-1, Num_Miners, Group_Prefix_Sha_Values, Curr_Member+1).

find_group(_,_,0,_,_) ->
  ok;

%Proof of Work Function of the Actor/Miner where the group is found
find_group(Group_Prefix_Sha_Values, Member_ID, Num_Iterations, Random_Number, Miner) ->
  %Computes Random String as Nonce and concatinates it with Member ID
  Nonce = base64:encode_to_string (crypto:strong_rand_bytes (rand:uniform (5) + Random_Number)),
  Random_String = concat (Member_ID, Nonce),

  %Computes Sha256 value of the Random String
  Sha_value = lists:last(io_lib:format ("~64.16.0b", [binary:decode_unsigned (crypto:hash (sha256, Random_String))])),
  %Stores the Prefix value of Sha256
  Prefix_value = substr(Sha_value, 1, 5),
  %Checks if any group ID matches with this prefix
  Match_found = orddict:is_key(Prefix_value, Group_Prefix_Sha_Values),

  if Match_found == true ->
    %Sends a message to server if a match is found else proof of work is continued
    server ! {group_found, Member_ID, orddict:fetch(Prefix_value, Group_Prefix_Sha_Values), Miner, Nonce};
    true ->
      find_group(Group_Prefix_Sha_Values, Member_ID, Num_Iterations-1, Random_Number, Miner)
  end.


start_actors(0,_,_,_,_) ->
  ok;

%Function to spawn the actors for a given set of miners
start_actors(Num_Miners, Group_Prefix_Sha_Values, Member_ID, Random_Number, Curr_Miner) ->
  %Computes Miner ID starting from 1 to Num_Miners
  Miner = concat("Miner_",integer_to_list(Curr_Miner)),
  %Spawns the proof of work actor for each miner
  spawn(main_server, find_group, [Group_Prefix_Sha_Values, Member_ID, 50000, Random_Number, Miner]),
  start_actors(Num_Miners-1, Group_Prefix_Sha_Values, Member_ID, Random_Number+10, Curr_Miner+1).

compute_sha_groups(0, Group_Sha_Values, Group_Prefix_Sha_Values, _) ->
  {Group_Sha_Values, Group_Prefix_Sha_Values};

%Function to assign ID to each group
compute_sha_groups(Number_Of_Groups, Group_Sha_Values, Group_Prefix_Sha_Values, Curr_Group_Num) ->
  %Computes random string for each group
  Group_Prefix = concat("Group_",integer_to_list(Curr_Group_Num)),
  Group_Num_Prefix = concat(Group_Prefix, "_"),
  Random_String = concat(Group_Num_Prefix,base64:encode_to_string (crypto:strong_rand_bytes (rand:uniform (10)))),

  %Computes Sha256 value of the Random String
  Sha_value = lists:last(io_lib:format ("~64.16.0b", [binary:decode_unsigned (crypto:hash (sha256, Random_String))])),

  %Checks if Sha256 value is calculated without any issues
  Valid_Sha_value = io_lib:char_list(Sha_value),
  if Valid_Sha_value == false ->
    compute_sha_groups(Number_Of_Groups, Group_Sha_Values, Group_Prefix_Sha_Values, Curr_Group_Num);
    true ->
      %Computes prefix of Sha value and stores in dictionaries
      Sub_string = substr(Sha_value, 1, 5),
      compute_sha_groups(Number_Of_Groups-1, orddict:store(Sha_value, Random_String, Group_Sha_Values), orddict:store(Sub_string, Sha_value, Group_Prefix_Sha_Values), Curr_Group_Num+1)
  end.


%Function to verify PoW
verify_nonce(Member_ID, Group_ID, Nonce, Verification_Pid) ->
  %Computes random string for each group
  Random_String = concat (Member_ID, Nonce),

  %Computes Sha256 value of the Random String
  Sha_value = lists:last(io_lib:format ("~64.16.0b", [binary:decode_unsigned (crypto:hash (sha256, Random_String))])),
  %Stores prefix value of Sha value
  Sha_Prefix_value = substr(Sha_value, 1, 5),
  %Stores prefix value of Group ID
  Group_Prefix_value = substr(Group_ID, 1, 5),

  %If both values match, then the PoW is valid
  if Sha_Prefix_value == Group_Prefix_value ->
    %Send a valid PoW message
    Verification_Pid ! {valid_nonce};
    true ->
      pass
  end.

start_verify_actors(_,_,_,0,_) ->
  ok;

%Function to spawn actors for verification phase
start_verify_actors(Member_ID, Group_ID, Nonce, Num_Miners, Verification_Pid) ->
  spawn(main_server, verify_nonce, [Member_ID, Group_ID, Nonce, Verification_Pid]),
  start_verify_actors(Member_ID, Group_ID, Nonce, Num_Miners-1, Verification_Pid).

verification(0) ->
  server ! {verification_done};

%Function to initiate the verification phase and checks if more than 50% send valid message
verification(Min_Miners) ->
  receive
    {valid_nonce} ->
      verification(Min_Miners-1)
  end.

compute_xor([], Xor_value) ->
  Xor_value;

%Function to compute xor value of all Group IDs
compute_xor([ShaValue | ShaValues], Xor_value) ->
  Bin_value = list_to_integer(integer_to_list(list_to_integer(ShaValue,16),2)),
  compute_xor(ShaValues, Bin_value bxor Xor_value).

%Function to find a winner group
find_winner_group(Group_Sha_Values, Group_Prefix_Sha_Values, Num_Miners) ->
  %Store all Group IDs in a list
  Sha_Values = orddict:fetch_keys(Group_Sha_Values),
  %Compute Xor Value of all Group IDs
  Xor_value = compute_xor(Sha_Values, 0),
  %Spawn actors to perform PoW to find winner group
  spawn(main_server, start_actors, [Num_Miners, Group_Prefix_Sha_Values, integer_to_list(Xor_value), 0, 1]).


%Main server function to handle all the functionalities
server_function(Group_Sha_Values, Group_Prefix_Sha_Values, Members_Group_Info, Num_Miners, Curr_Group_Num)->

  receive
    %To create groups
    {create_groups, Number_Of_Groups} ->
      {Group_Sha_Values_,Group_Prefix_Sha_Values_}  = compute_sha_groups(Number_Of_Groups, [], [], Curr_Group_Num),
      io:format("Group IDs and Names ~n ~p ~n",[Group_Sha_Values_]),
      io:format("~n ~n Prefix and IDs ~n ~p ~n",[Group_Prefix_Sha_Values_]),
      io:format("Groups Created ~n"),
      server_function(Group_Sha_Values_, Group_Prefix_Sha_Values_, Members_Group_Info, Num_Miners, Curr_Group_Num+Number_Of_Groups);

    %To add additional groups to existing list
    {add_groups, Number_Of_Groups} ->
      {Group_Sha_Values_,Group_Prefix_Sha_Values_}  = compute_sha_groups(Number_Of_Groups, Group_Sha_Values, Group_Prefix_Sha_Values, Curr_Group_Num),
      io:format("Group IDs and Names ~n ~p ~n",[Group_Sha_Values_]),
      io:format("~n ~n Prefix and IDs ~n ~p ~n",[Group_Prefix_Sha_Values_]),
      io:format("Groups Added ~n"),
      server_function(Group_Sha_Values_, Group_Prefix_Sha_Values_, Members_Group_Info, Num_Miners, Curr_Group_Num+Number_Of_Groups);

    %Assign group to a given Member
    {assign_group, Member_ID} ->
      %Checks if Member is already assigned
      Check_if_group_assigned = orddict:is_key(Member_ID, Members_Group_Info),
      if Check_if_group_assigned ->
        %If group already assigned, print and return
        Group_Name = orddict:fetch(Member_ID, Members_Group_Info),
        io:format("Member ~p already assigned to the group ~p ~n",[Member_ID,Group_Name]);
        true ->
        spawn(main_server, start_actors, [Num_Miners, Group_Prefix_Sha_Values, Member_ID, 0, 1])
      end,
      server_function(Group_Sha_Values, Group_Prefix_Sha_Values, Members_Group_Info, Num_Miners, Curr_Group_Num);

    %Assign groups for a given number of members
    {assign_groups, Number_Of_Members} ->
      assign_groups(Number_Of_Members, Num_Miners, Group_Prefix_Sha_Values, 1),
      server_function(Group_Sha_Values, Group_Prefix_Sha_Values, Members_Group_Info, Num_Miners, Curr_Group_Num);

    %Matching Group Found for a given Member ID
    {group_found, Member_ID, Group_ID, Miner, Nonce} ->
      Check_if_group_assigned = orddict:is_key(Member_ID, Members_Group_Info),
      if Check_if_group_assigned ->
        %If member is already assigned to a group by a different miner, then return
        server_function(Group_Sha_Values, Group_Prefix_Sha_Values, Members_Group_Info, Num_Miners, Curr_Group_Num);
        true ->
          Group_Name = orddict:fetch(Group_ID, Group_Sha_Values),
          io:format("Member ~p assigned to Group ~p by Miner ~p ~n",[Member_ID, Group_Name, Miner]),

          %Verify if PoW is valid. Min_Miners will set a minimum required valid responses
          Min_Miners = ceil(0.5*Num_Miners) + 1,
          Verification_Pid = spawn(main_server, verification, [Min_Miners]),
          spawn(main_server, start_verify_actors, [Member_ID, Group_ID, Nonce, Num_Miners-1, Verification_Pid]),

          receive
            {verification_done} ->
              io:format("Member ~p assignment to Group ~p is verified by other Miners ~n ~n",[Member_ID, Group_Name])
          end,
          server_function(Group_Sha_Values, Group_Prefix_Sha_Values, orddict:store(Member_ID, Group_Name, Members_Group_Info), Num_Miners, Curr_Group_Num)
      end;

    %Find Winner group
    {winner_group} ->
      find_winner_group(Group_Sha_Values, Group_Prefix_Sha_Values, Num_Miners),
      receive
        {group_found, Member_ID, Group_ID, Miner, Nonce} ->
          Group_Name = orddict:fetch(Group_ID, Group_Sha_Values),
          io:format("Group ~p is picked as Winner by Miner ~p ~n",[Group_Name, Miner]),

          %verification
          Min_Miners = ceil(0.5*Num_Miners) + 1,
          Verification_Pid = spawn(main_server, verification, [Min_Miners]),
          spawn(main_server, start_verify_actors, [Member_ID, Group_ID, Nonce, Num_Miners-1, Verification_Pid]),

          receive
            {verification_done} ->
              io:format("Group ~p Winner is verified by other Miners ~n ~n",[Group_Name])
          end,
          server_function(Group_Sha_Values, Group_Prefix_Sha_Values, orddict:store(Member_ID, Group_Name, Members_Group_Info), Num_Miners, Curr_Group_Num)
      end,
      server_function(Group_Sha_Values, Group_Prefix_Sha_Values, Members_Group_Info, Num_Miners, Curr_Group_Num);

    %Add additional Miners
    {add_miners, New_Miners} ->
      io:format("Miners added. New Miners count is ~p ~n",[Num_Miners+New_Miners]),
      server_function(Group_Sha_Values, Group_Prefix_Sha_Values, Members_Group_Info, Num_Miners+New_Miners, Curr_Group_Num)
  end.

start() ->
  % Registering server_function as server and initializing the values
  register(server, spawn(main_server, server_function, [[],[],[],5,1])).
