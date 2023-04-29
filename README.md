# Pooled Randomization

## Description
This project is a simulation using Erlang to show the results of Pooled Randomization using Blockchain Technology

## Running the application
1) Install erlang from https://www.erlang.org/downloads and clone this repository.

2) Open terminal and cd to the cloned directory and run "erl" to enter into erlang shell.

3) To compile and start the server, run the following:

```bash
 c(main_server).
 main_server:start().
```

4) To create groups, run the following:

```bash
server!{create_groups,[Num_Groups]}.
```
- ```Num_Groups``` is the number of groups
- Successful execution will print Group Sha Values and it's respective Group Names as below
<img width="524" alt="image" src="https://user-images.githubusercontent.com/52484406/235328258-e056a51e-7949-4a6a-9fb7-cfcf6d3e98be.png">

5) To add additional groups, run the following:

```bash
server!{add_groups,[Additional_Groups]}.
```
- ```Additional_Groups``` is the additional number of groups required

6) To assign group for a given member ID, run the following:

```bash
server!{assign_group,[Member_ID]}.
```
- ```Member_ID``` is the ID of the member to be assigned to a random group
- Successful execution will print the assigned Group Name along with the Miner who was able to perform PoW and the verification results
- We can see below that each time the Group assignment is random
<img width="714" alt="image" src="https://user-images.githubusercontent.com/52484406/235328334-5de1cacf-0f87-43b3-b93c-eed9edbc8b14.png">

7) To assign group for a set of members, run the following:

```bash
server!{assign_groups,[Num_Members]}.
```
- ```Num_Members``` is the number of Members to be assigned to random groups
- Successful execution will print the assigned Group Name for each Member along with the Miner who was able to perform PoW and the verification results
- We can see below that the Group assignment and Miner who was able to perform valid PoW is random
<img width="768" alt="image" src="https://user-images.githubusercontent.com/52484406/235328368-8618b492-7459-4573-af5c-a48d87247c51.png">

8) To find a winner group, run the following:
```bash
server!{winner_group}.
```
- Successful execution will print the Winner Group Name along with the Miner who was able to perform PoW and the verification results
- We can see below that the Winner group is random in each iteration along with the Miner who performed PoW
<img width="505" alt="image" src="https://user-images.githubusercontent.com/52484406/235328448-7522b299-8285-401c-8bb5-562ca90acf89.png">

9) To add additional Miners, run the following:
```bash
server!{add_miners, [Additional_Miners]}.
```
- ```Additional_Miners``` is the number of additional Miners
<img width="343" alt="image" src="https://user-images.githubusercontent.com/52484406/235328492-9a9df3a1-2dbf-4a5e-b23f-46b5ebfa6a2b.png">
