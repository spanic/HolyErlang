# Erlang Training Camp Tasks

### Task №1 (since 03.10) — Recursion

* **03.10** | Implemented n-th Fibonacci number & factorial calculations;
* **04.10** | Added some guards to prevent using incorrect arguments;

### Task №2 (since 05.10) — Erlang Basics

* **07.10** | Added solutions for ex. 1.5 (some tasks had been implemented in a several different ways):
    * **Cartesian join** of two lists;
    ```
    c(homework).
    
    homework:cartesian([A], [B]).
    ```
    * **List flattening** (i. e. «unpacking» inner elements);
    ```
    homework:flatten(List).
    ```
    * **Creating a list of «tags»** from the source list containing maps like `#{tags => [...]}`;
    ```
    homework:get_tags(List).
    ```
    * **Getting all tuples** from the input list;
    ```
    homework:get_tuples(List).
    ```
    * **Getting all rectangles** that has area **lower than N** from the list like `Shapes = [{{0, 0}, {10, 10}}, ...]`;
    ```
    homework:validate_areas(Shapes).
    ```
    * **Serializing & deserializing** previously mentioned list of rectangles to/from the binary format;
    ```
    serialize_shapes(Shapes).
    
    deserialize_shapes(BinaryShapes).
    ```
    