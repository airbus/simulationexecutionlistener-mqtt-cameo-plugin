
# Introduction

This project is the source code for the `SimulationExecutionListener MQTT Cameo Plugin`, a Java plug-in written for Cameo Systems Modeler (CSM) and MagicDraw (MD), a Dassault Systèmes software. It enables connecting the simulation of models in the tool to external applications by implementing the [SimulationExecutionListener](https://docs.nomagic.com/display/CST2022x/Creating+and+registering+a+new+SimulationExecutionListener). All communication is based on [MQTT](https://mqtt.org/). State changes and variable changes in the model are sent to the MQTT broker and messages received from the MQTT broker trigger injection of events into and setting variable values in the model.

## License

Refer to [LICENCE](./LICENSE.md) file.

## Contributing

Contributions are what make the open source community such an amazing place to learn, inspire, and create. Any contributions you make are **greatly appreciated**. For detailed contributing guidelines, please see [CONTRIBUTING.md](CONTRIBUTING.md)

# Build and Develop

## Build Dependencies
The used libraries are included in the lib folder.

## MagicDraw/Cameo Build Dependencies

* Additionally, building the plugin requires MagicDraw's OpenAPI libraries
* The MagicDraw's OpenAPI libraries are distributed with MD or CSM
* Building the plug-in has been tested with Cameo Systems Modeler 2022x

## Setting up Classpath for Development using Eclipse
Please follow the directions provided by the vendor at https://docs.nomagic.com/display/MD2022x/Development+in+Eclipse to set the classpath for MagicDraw's OpenAPI bundled with your version of MD or CSM.

# Installation and Usage

## Install from Source

- Export the project as a .jar file called `simulationexecutionlistener-mqtt-cameo-plugin.jar`.

- Create a new folder `simulationexecutionlistener-mqtt-cameo-plugin` in `C:/Users/$USER/AppData/Local/.cameo.systems.modeler/2022x/plugins`.

- Copy the `simulationexecutionlistener-mqtt-cameo-plugin.jar` file, the `custom_window.png` file, the `plugin.config` file, the `plugin.xml` and - if necessary - a certificate file for secure communications file into the folder `C:/Users/$USER/AppData/Local/.cameo.systems.modeler/2022x/plugins/simulationexecutionlistener-mqtt-cameo-plugin`.

- If necessary, adapt the `plugin.config` file to, e.g., set the broker address.

- Create a `lib` folder in `C:/Users/$USER/AppData/Local/.cameo.systems.modeler/2022x/plugins/simulationexecutionlistener-mqtt-cameo-plugin` and copy the required runtime libraries (bcprov-jdk18on-175.jar, json-20230618.jar and org.eclipse.paho.mqttv5.client-1.2.5.jar) into it.

- Start Cameo.

## Install from Release

- Unzip the `simulationexecutionlistener-mqtt-cameo-plugin.zip` into `C:/Users/$USER/AppData/Local/.cameo.systems.modeler/2022x/plugins`.

- If necessary, adapt the `plugin.config` file in `C:/Users/$USER/AppData/Local/.cameo.systems.modeler/2022x/plugins/simulationexecutionlistener-mqtt-cameo-plugin` to, e.g., set the broker address.

- Start Cameo.

## Configuration
The plugin is configured using the plugin.config YAML file that must be in the same directory as the plugin itself. If the plugin.config is missing or any of the entries are missing, the default values are used.

|   Entry    |         Purpose         | Default |
| :----------: | :---------------------: | :-----: |
| broker.name | Name or IP address of the MQTT broker |  localhost  |
| broker.port | Port number for the MQTT connection |  1883  |
| broker.protocol | Protocol used for MQTT connection; can either be tcp or wss (secure Websockets)        |  tcp  |
| security.cafilename | Name of the certificate file that is used for the connection using secure Websockets |  ca.crt  |
| clientid | Prefix for the name that the plugin uses to connect to the MQTT broker | SimulationExecutionListenerMQTTCameoPlugin  |
| qos | MQTT quality of service |  0  |
| maxreconnectattempts| Number of connection attempts to the MQTT broker before goving up |  5  |
| plugin.gui | If true, a window is added to the Cameo GUI on project load |  true  |
| plugin.enabled | Plugin is only started when enabled is set to true |  true  |

## Use

During simulation state change events are automatically sent to the MQTT broker

## Topics and content

### Publish

The plugin publishes information on the following topics:

- model/execution/active

true, if the model simulation in CSM is active

false, if the model simulation in CSM is not active
- model/object

Anytime an object is created in the model simulation.

Example:
```javascript
{
  "id": "SwitchButton@5a5a2aad",
  "type": {
    "qualifiedName": "OBOE Sandbox::SimpleSystem::Components::SwitchButton",
    "name": "SwitchButton",
    "metaclass": {
      "stereotypes": ["Block"],
      "simpleName": "Class",
      "fullName": "com.nomagic.uml2.ext.magicdraw.classes.mdkernel.Class"
    }
  }
}
```
- model/state/activated

Anytime an object changes to a new state.

Example:
```javascript
{
  "state": "OBOE Sandbox::SimpleSystem::Components::LED::LED::::on::::GREEN",
  "object": "LED@59177408"
}
```
- model/state/deactivated

Anytime an object leaves a state.

Example:
```javascript
{
  "state": "OBOE Sandbox::SimpleSystem::Components::LED::LED::::on::::ORANGE",
}
```
- model/variables/values

Anytime the value of a value property of an object changes.

Example:
```javascript
{
  "property": "OBOE Sandbox::SimpleSystem::Components::Controller::amountWorked",
  "value": "[1]",
  "object": "Controller@1dfd36ca"
}
```

### Subscribe

The plugin subscribes to the following topics:

- model/signals/create

Creates a signal and sends it to the specified target.

The target can either be the name of the class of the target, then the signal will be sent to the first object of that class type.
Or the target can be specified using the dot notation, where the first element will be the name of a class and the following names are the names of properties.

Example:
```javascript
{
  "signal": {
    "name" : "signalWithBoolValue",
    "payload" : {
      "boolValue" : false
    }
  },
  "target": "TestBlock"
}

{
  "signal": {
    "name" : "evtPushButtonPressed"
  },
  "target": "SmartHomeContext.SmartHome_SOI.pushButton2"
}
```

- model/variables/set

Sets the specified variable of the target to the specified value.

The lookup of the target works like the the one for the signal creation described above.

Example:
```javascript
{
  "variable": {
    "name" : "complexValue",
    "value" : {
      "value1": "test",
      "value2": 3
    }
  },
  "target": "TestBlock"
}
```

## Debug

- Check `csm.log` in `C:/Users/$USER/AppData/Local/.cameo.systems.modeler/2022x`

# Publication and Citation

More details can be found in the article [`Hardware-in-the-Loop with SysML and Cameo Systems Modeler`][4], which has been published at the INCOSE's 34th Annual International Symposium 2024.

If you use this software, please cite it as below.
```bibtex
@article{Helle_Hardware-in-the-Loop_with_SysML_2024,
author = {Helle, Philipp and Schramm, Gerrit},
doi = {10.1002/iis2.13238},
journal = {INCOSE International Symposium},
month = sep,
number = {1},
pages = {1807--1819},
title = {{Hardware-in-the-Loop with SysML and Cameo Systems Modeler}},
volume = {34},
year = {2024}
}
```

# Contact

Philipp Helle - philipp.helle@airbus.com

Gerrit Schramm - gerrit.schramm@airbus.com

# Dependencies
 ## Runtime
|   Library    |         Purpose         | Release |
| :----------: | :---------------------: | :-----: |
| [Eclipse Paho][1] |        Required (MQTT)        |  1.2.5  |
| [JSON in Java][2] |        Required (JSON)       |  20230618  |
| [Bouncycastle][3] |        Required (SSL)        |  r1rv75  |

<!-- Links -->
[1]: https://github.com/eclipse/paho.mqtt.java
[2]: https://github.com/stleary/JSON-java
[3]: https://github.com/bcgit/bc-java
[4]: https://www.researchgate.net/publication/383858786_Hardware-in-the-Loop_with_SysML_and_Cameo_Systems_Modeler
