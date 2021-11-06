# Aim of the language

Allow for highly modular design, similar to putting the system together from lego bricks.

# Keywords with code examples
## `dflow`
```
dflow name {
  stage_1(args) -> outs
  .
  .
  .
  stage_n(args) -> outs
}
```

Example:

```
dflow ControlledProcess {
  state_collector() -> StateInfo;
  controller_input_collector() -> ControllerInput
  controller(StateInfo, ControllerInput) -> ControllerOutput;
  process_interface(ControllerOutput);
};
```

Explanation:
1. Stages are identifiers. While implementing a component or another data flow is assigned to a stage
2. Types used in args and outs are there to only to connect the flow, they don't exist as actual types. Args and outs are connected to each other during definition to check whether the flow is properly connected, i.e. there are no hanging outs or inexistent args. When implemented, the compatibility of component ins and outs is checked.


## `compnt`
```
compnt name {
  Init(args)
  : pre (conds)
  : post (conds)
  {

  }
  Call(args) -> outs
  : pre (conds)
  : post (conds){
  }

  m {
    methods
    members
  }
}
```

Example:

```
compnt Gps {
  Init(ip: String)
  : pre (
    IsValidIp(ip)
  )
  : post (
    m.IsConnected()
  ) {
    
  }

  Call() -> GpsOutput{
    return GpsOutput(
      position(0, 0, 0);
      orientation_rpy(0, 0, 0);
    );
  }

  IsConnected() -> bool {
    return true;
  }
} 
```

## `struct`

```
struct name {
  identifier_1: type_1
  .
  .
  .
  identifier_n: type_n
}
```

Example: 

```
struct GpsOutput {
  position: Vector<f32, 3>;
  orientation_rpy: Vector<f32, 3>;
}
```

## `module`
```
module name {
  dflows
  components
  structs
}
```

## `impl`

```
impl dflow_name as impl_name {
  stage_1: component_or_dflow_1;
  .
  .
  .
  stage_n: component_or_dflow_n;
}
```

Example:

```
impl ControlledProcess as Drone {
  state_collector: DroneStateCollector,
  controller_input_collector: ReferencePathServer;
  controller: MpcController;
  process_interface: MotorInterface;
};
```

# Standard library

Standard library provides the following basic types

```
String
Vector<type, size>
Vector<type>
i32
f32
u32
u8
bool
char
```

# Control flow

## `if`

```
if boolean_expr_1 {
  
} else if boolean_expr_2 {
.
.
.
} else if boolean_expr_n {

} else {

}
```

## `for`

```
for element in vector {

} 

for element, idx in vector {

}

for idx in range(from, to exclusive, increment) {

}
```

## `while`
```
while boolean_expr {

}
```
# Mutability

Everything is immutable by default. Use `mut` keyword to allow for mutating component members. You can't use `mut` for function arguments. Arguments of `Call` component function are moved as they're just moving through a data flow and are no longer needed in the stage before the component.

# Assignment

Data structures are moved during assignment. basic types like i32 and char are copied. 

# References

Immutable references are used when passing data structures to a function.

# Function libraries and design by contract

Functions outside components can be defined in libraries that support components in generic functionalities. Library functions can't have any side effects. Functions can call other functions including themselves. You can declare private functions that aren't visible outside the library for helper functions that don't have to be visible to the library user. In functions without side effects there are no class invariants to check after the function execution, but you can put requirements for the output in the `post` block. Also use of the assert statement is encouraged to check conditions that should never happen.

```
library name {
  fn name(args) -> outs {

  }
}
```
Example:

```
library ip {

fn IsCorrectIp(ip: String) -> bool {
  if IsKnownInvalidAddresses(ip) {
    return false;
  }
  let ip_numbers = NumberVectorFromIpString(ip);
  for segment in ip_numbers {
    if (segment > 255 or segment < 0) {
      return false;
    }
  }

  return true;
}

priv fn IsKnownInvalidAddresses(ip: String) {
  if ip == "0.0.0.0" {
    return false;
  } else if ip == "255.255.255.255" {
    return false;
  }
}

priv fn NumberVectorFromIpString(ip: String) -> Vector<u8, 4>
: post(numbers.len() == 4) {
  let substrings = ip.Split('.');

  let numbers = Vector<u8, 4>::New();
  for s,idx in substrings {
    numbers[idx] = s.To_u32();
  }
  return numbers;
}

}
```

## Binding function libraries to data structures

As a compromise between object oriented and procedural programming, you can create a library and then if all of the non private functions take the type of a given data structure as a first argument, you can bind the library to the data structure and later use it like `ip.Split('.')` in the example above. That's how the String type is implemented.

```
struct StructName {
  bind lib_name;
  indentifier_1: type_1;
  .
  .
  .
  indentifier_n: type_n;
}

library lib_name {
  fn Fn1(s: StructName, args) -> outs{

  }
  .
  .
  .
  fn FnN(s: StructName, args) -> outs{

  }

  priv fn PrivFn1(args){

  }
  .
  .
  .
  priv fn PrivFn1(args){

  }

}

```

# How to run a program

You just write
```
main(dflow_instance)
```
And that's it.



