# Erlang Training Camp Tasks

### Task №1 (since 03.10) — Recursion

* **03.10** | Implemented n-th Fibonacci number & factorial calculations;
* **04.10** | Added some guards to prevent using incorrect arguments. (see `experiments` module)

### Task №2 (since 05.10) — Erlang Basics

* **07.10** | Added solutions for **ex. 1.5** (some tasks had been implemented in a several different ways), try 
`c(homework), homework:module_info().`;
* **08.10** | Added solutions for **ex. 1.6** and **1.7** (except the last two tasks about DNA), see `boolean` and 
`homework` modules;
* **10.10** | Solved **first** task about DNA processing from **ex. 1.7**;
* **11.10** | Completed the **last task** from **1.7** (both of them were added to `homework` module);
* **15.10** | Added solution for **ex. 1.8** (with additional values validation) (see `json` module)
   
### Task №3 (since 10.10) — Functional Programming Techniques

* **11.10** | Added `lists:map/2` and `lists:filter/2` implementations via `lists:foldl/3` function (see `experiments` 
module)

### Task №4 (since 12.10) — BIFs and Libraries Overview

* **16.10** | Implemented operations with files as a preparation for **ex. 2.0** (see `database` module);
* **23.10** | Finished **ex. 2.0** with advanced saving DB to file functionality;
* **24.10** | Added new DB format support for **ex. 2.1** (for now it works only with properties parsing & validation).
**Finished DB properties support** implementation, **added new batch methods** & verification of append possibility (
see `append/3`, `batch_delete/2` and `batch_read/2` in `database` module);
* **30.10** | Added **lazy lists** converter and simple mapping & filtering lazy operations for **ex. 2.5.1** 
(see `homework` module);
* **01.10** | Implemented **lazy lists concatenation** in two different ways (`lazy_concat/2`, `s_lazy_concat/2`) 
and **LL unwrapping** (used for solution testing);
* **04.11** | Added **LL-based reading from file** (`../resources/strings.txt` by default, but you can specify the path 
by yourself via `lazy_read/1`) as a part of **ex. 2.5.1**. Try to call `Test = homework:lazy_read(). Test().` to see 
how it works

### Task №5 (since 17.10) – Actors model

* **06.11** | Implemented **"Not" logical element** based on the @bitgorbovsky's `gen_proc` and `wire` modules, see 
`circuits\le_not.erl` file, added unit tests

_Work in progress..._