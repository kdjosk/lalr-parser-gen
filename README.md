# Data flows and components.
## Data flow
Data flow describes a path that data takes through a system of abstract stages. The data should be consumed by the last element of the flow. Types used in the flow definition are purely abstract and don't actually exist. Let's say we want to write a program for a drone. We could start with:

```
dflow Drone {
  drone_state_collector() -> StateInfo;
  controller_input_collector() -> ControllerInput;
  controller(StateInfo, ControllerInput) -> Force;
  motor_interface(Force);
};
```

An example implementation of the Drone data flow could be

```
impl Drone as AutonomousDrone (
  drone_state_collector: DroneStateCollector;
  controller_input_collector: ReferencePathServer;
  controller: MpcController;
  motor_interface: MotorInterface;
);
```

But also nothing stops you from using this data flow to control an oven (or any other process). 

```
impl Drone as Oven (
  drone_state_collector: OvenThermometer;
  controller_input_collector: TemperatureGaugeState;
  controller: PidController;
  motor_interface: OvenPowerController;
);
```

This fact suggests that the data flow could be made a bit more abstract, e.g.

```
dflow ControlledProcess {
  state_collector() -> StateInfo;
  controller_input_collector() -> ControllerInput;
  controller(StateInfo, ControllerInput) -> ControllerOutput;
  process_interface(ControllerOutput);
};
```
That looks better!

Now we can implement a drone and an oven with this data flow and it doesn't seem strange in any way.
 
```
impl ControlledProcess as Drone {
  state_collector: DroneStateCollector;
  controller_input_collector: ReferencePathServer;
  controller: MpcController;
  process_interface: MotorInterface;
};
```

It seems that if you want to implement a concrete system you have to devise an abstract flow of data through it. But this approach to designing your system will help you build a modular system where each component has a clearly defined responsibility.

## Components in data flows
A data flow can be instantiated by implementing it first i.e. assigning component or data flow types to stages, and then instanced by assigning correct component or data flow instances to stages. The compiler will make sure the types are correctly assigned.

```

### Instantiation following implementation

```
impl ControlledProcess as Drone {
  state_collector: DroneStateCollector,
  controller_input_collector: ReferencePathServer;
  controller: MpcController;
  process_interface: MotorInterface;
};

let drone = Drone(
  state_collector=DroneStateCollector(
    sensors=sensors,
  ),
  controller_input_collector=ReferencePathServer(
    serialized_path=path,
  ),
  controller=MpcController(
    N=10,
    Nu=10,
    no_vars=drone_state_vars,
    weights=weights,
  ),
  process_interface=MotorInterface(),
)

```

## Components 101

Components are defined in modules. In a module you can have many similar components. Components are atomic in a way that components can't be part of other components. Components can only be parts of a data flow. You can compose data flows from components or other data flows. All components have only one way of interacting with the outside world, they can be called by the () operator. You can pass arguments in the round brackets and you can get a return value. 

Simple and obvious components like gps and imu sensors can be implemented like this

```
module sensors {

from ip import IsValidIp

struct GpsOutput {
  position: Vector<f32, 3>;
  orientation_rpy: Vector<f32, 3>;
}

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

struct ImuOutput {
  acceleration: Vector<f32, 3>;
  angular_accel: Vector<f32, 3>;
}

compnt Imu {
  Init(ip: String)
  : pre (
    IsValidIp(ip)
  )
  : post (
    m.IsConnected()
  ) {
    
  }

  Call() -> ImuOutput{
    return ImuOutput(
      position(0, 0, 0);
      orientation_rpy(0, 0, 0);
    );
  }

  IsConnected() -> bool {
    return true;
  }
} 

}
```

Now let's consider the DroneStateCollector. It takes data from imu and gps and puts it in a data structure, so as there's data flowing through it we can define it as a data flow.
```
dflow GpsImuStateCollector {
  gps() -> Position, Orientation;
  imu() -> LinearAccel, AngularAccel;
  sensor_fusion(Position, Orientaion, Velocity, Angular) -> StateInfo;
}
```

```
module drone_state_collector {
struct StateInfo {
  position: Vector<f32, 3>;
  orientation_rpy: Vector<f32, 3>;
  acceleration: Vector<f32, 3>;
  angular_accel: Vector<f32, 3>;
};

impl GpsImuStateCollector as DroneStateCollector {
  gps: Gps;
  imu: Imu;
  sensor_fusion: GpsImuToStateInfo;
};

}
```


