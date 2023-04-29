# Pooled Randomization


## Running the application
1) Install erlang from https://www.erlang.org/ and clone this repository.

2) Install erlang tools.


3) To start the server(boss), run the following:

```bash
erl -make
erl -name [boss_identifier]@[boss_ip] -pa ebin
> application:start(bitcoin).
> app:start(5).
```

- ```boss_identifier``` is a unique identifier for the boss and ```boss_ip``` is the ip address of the machine the boss is running on.
- ```start``` takes the number of leading zeros desired in a coin.

4) To start the worker, run the following:

```bash
erl -make
erl -name [worker_identifier]@[worker_ip] -pa ebin
> app:start('[boss_identifier]@[boss_ip]').
```
- ```worker_identifier``` is a unique identifier for the worker and ```worker_ip``` is the ip address of the machine the boss is running on.
- ```start``` takes the boss identifier and ip from step 3.
