/*
* Copyright (c) 2025 AIRBUS and its affiliates.
* This Source Code Form is subject to the terms of the Mozilla Public
* License, v. 2.0. If a copy of the MPL was not distributed with this
* file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

package com.airbus.mqttconnector;

public class SimulationExecutionListenerMQTTCameoPluginConstants {

	private SimulationExecutionListenerMQTTCameoPluginConstants() {

	}

	//MQTT topic related
	public static final String MODEL_TOPIC_PREFIX = "model";

	public static final String MODEL_OBJECTS_TOPIC = MODEL_TOPIC_PREFIX + "/objects";

	public static final String MODEL_STATE_TOPIC_PREFIX = MODEL_TOPIC_PREFIX + "/states";
	public static final String MODEL_STATE_ACTIVATED_TOPIC = MODEL_STATE_TOPIC_PREFIX + "/activated";
	public static final String MODEL_STATE_DEACTIVATED_TOPIC = MODEL_STATE_TOPIC_PREFIX + "/deactivated";

	public static final String MODEL_VARIABLE_TOPIC_PREFIX = MODEL_TOPIC_PREFIX + "/variables";
	public static final String MODEL_VARIABLE_VALUE_TOPIC = MODEL_VARIABLE_TOPIC_PREFIX + "/values";
	public static final String MODEL_VARIABLE_SET_TOPIC = MODEL_VARIABLE_TOPIC_PREFIX + "/set";

	public static final String MODEL_SIGNALS_TOPIC_PREFIX = MODEL_TOPIC_PREFIX + "/signals";
	public static final String MODEL_SIGNALS_CREATE_TOPIC = MODEL_SIGNALS_TOPIC_PREFIX + "/create";
	public static final String MODEL_SIGNALS_EXIST_TOPIC = MODEL_SIGNALS_TOPIC_PREFIX + "/exist";

	public static final String MODEL_EXECUTION_TOPIC_PREFIX = MODEL_TOPIC_PREFIX + "/execution";
	public static final String MODEL_EXECUTION_ACTIVE_TOPIC = MODEL_EXECUTION_TOPIC_PREFIX + "/active";


	//Config related
	public static final String PLUGIN_CONFIG = "plugin.config";
	
	public static final String BROKER_NAME = "broker.name";
	public static final String DEFAULT_BROKER_NAME = "localhost";

	public static final String BROKER_PORT = "broker.port";
	public static final int DEFAULT_BROKER_PORT = 1883;

	public static final String PROTOCOL_NAME = "broker.protocol";
	public static final String DEFAULT_PROTOCOL_NAME = "tcp";

	public static final String WSS = "wss";
	
	public static final String SECURITY_CAFILE_NAME = "security.cafilename";
	public static final String DEFAULT_SECURITY_CAFILE_NAME = "ca.crt";

	public static final String CLIENT_ID = "clientid";
	public static final String DEFAULT_CLIENT_ID = "SimulationExecutionListenerMQTTCameoPlugin";

	public static final String QOS = "qos";
	public static final String MAX_RECONNECT_ATTEMPTS = "maxreconnectattempts";

	public static final String PLUGIN_GUI = "plugin.gui";
	public static final String PLUGIN_ENABLED = "plugin.enabled";

	//JSON fields
	public static final String OBJECT = "object";
	public static final String STATE = "state";
	public static final String PROPERTY = "property";
	public static final String VALUE = "value";
	public static final String NA = "N/A";
	public static final String SIMPLE_NAME = "simpleName";
	public static final String FULL_NAME = "fullName";
	public static final String STEREOTYPES = "stereotypes";
	public static final String METACLASS2 = "metaclass";
	public static final String QUALIFIED_NAME = "qualifiedName";
	public static final String NAME = "name";
	public static final String ID = "id";
	public static final String TYPE = "type";
	public static final String TARGET = "target";
	public static final String SIGNAL = "signal";
	public static final String VARIABLE = "variable";
	public static final String PAYLOAD = "payload";

	//CAMEO
	public static final String CUSTOMIZATION_CLASS_NAME = "com.nomagic.magicdraw.simulation.fuml.customization.ao";
	
	//UI
	public static final String CONNECTED = "Connected";
	public static final String DISCONNECTED = "Disconnected";
}
