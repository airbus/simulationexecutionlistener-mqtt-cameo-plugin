/*
* Copyright (c) 2025 AIRBUS and its affiliates.
* This Source Code Form is subject to the terms of the Mozilla Public
* License, v. 2.0. If a copy of the MPL was not distributed with this
* file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

package com.airbus.mqttconnector;

import java.util.ArrayList;
import java.util.logging.Logger;

import javax.net.ssl.SSLSocketFactory;

import org.apache.commons.lang3.exception.ExceptionUtils;
import org.eclipse.paho.mqttv5.client.IMqttToken;
import org.eclipse.paho.mqttv5.client.MqttAsyncClient;
import org.eclipse.paho.mqttv5.client.MqttCallback;
import org.eclipse.paho.mqttv5.client.MqttConnectionOptions;
import org.eclipse.paho.mqttv5.client.MqttDisconnectResponse;
import org.eclipse.paho.mqttv5.client.persist.MemoryPersistence;
import org.eclipse.paho.mqttv5.common.MqttException;
import org.eclipse.paho.mqttv5.common.MqttMessage;
import org.eclipse.paho.mqttv5.common.MqttPersistenceException;
import org.eclipse.paho.mqttv5.common.MqttSecurityException;
import org.eclipse.paho.mqttv5.common.packet.MqttProperties;
import org.json.JSONObject;

import com.airbus.mqttconnector.utils.MQTTUtils;
import com.airbus.mqttconnector.utils.ModelUtils;
import com.airbus.mqttconnector.utils.PropertyLoader;
import com.airbus.mqttconnector.utils.SslUtils;
import com.nomagic.magicdraw.simulation.fuml.fUMLHelper;
import com.nomagic.uml2.ext.jmi.helpers.ClassifierHelper;
import com.nomagic.uml2.ext.magicdraw.classes.mdkernel.Property;

import fUML.Semantics.Classes.Kernel.Object_;

public class MQTTConnector {

	public static final Logger log = Logger.getLogger(MQTTConnector.class.getName());

	int qos = 0;
	
	MemoryPersistence persistence = new MemoryPersistence();
	MqttAsyncClient mqttClient = null;
	private String broker;
	private String clientId;
	private String protocol;
	private int brokerPort;
	private int countConnectionAttempts = 0;
	private int maxReconnectAttempts;
	private MqttConnectionOptions connOpts;
	private String pathToPlugin;
	private CameoSimulationExecutionListener listener;
	private boolean connectionInProgress = false;
	
	private void setBroker(String broker) {
		this.broker = protocol + "://" + broker + ":" + brokerPort;
	}
	
	public void connectwithNewBroker(String broker) {
		setBroker(broker);
		initClient();
		connect();
	}
	
	public void init(CameoSimulationExecutionListener listener, String pathToPlugin) {
		log.fine("init:begin");
		protocol = PropertyLoader.getInstance().getPropertyWithDefaultValue(SimulationExecutionListenerMQTTCameoPluginConstants.PROTOCOL_NAME, SimulationExecutionListenerMQTTCameoPluginConstants.DEFAULT_PROTOCOL_NAME);
		brokerPort = PropertyLoader.getInstance().getIntPropertyWithDefaultValue(SimulationExecutionListenerMQTTCameoPluginConstants.BROKER_PORT, SimulationExecutionListenerMQTTCameoPluginConstants.DEFAULT_BROKER_PORT);
		setBroker(PropertyLoader.getInstance().getPropertyWithDefaultValue(SimulationExecutionListenerMQTTCameoPluginConstants.BROKER_NAME, SimulationExecutionListenerMQTTCameoPluginConstants.DEFAULT_BROKER_NAME));
		clientId = PropertyLoader.getInstance().getPropertyWithDefaultValue(SimulationExecutionListenerMQTTCameoPluginConstants.CLIENT_ID, SimulationExecutionListenerMQTTCameoPluginConstants.DEFAULT_CLIENT_ID).concat("-").concat(MQTTUtils.generateRandomNumberString(6));
		qos = PropertyLoader.getInstance().getIntPropertyWithDefaultValue(SimulationExecutionListenerMQTTCameoPluginConstants.QOS, 0);
		maxReconnectAttempts = PropertyLoader.getInstance().getIntPropertyWithDefaultValue(SimulationExecutionListenerMQTTCameoPluginConstants.MAX_RECONNECT_ATTEMPTS, 5);
		this.pathToPlugin = pathToPlugin;
		this.listener = listener;
		initClient();
		connect();
		log.fine("init:begin");
	}
	
	public boolean isConnected() {
		return mqttClient.isConnected();
	}
	
	public void connect() {
		while (!connectionInProgress && !mqttClient.isConnected()) {
			if (countConnectionAttempts < maxReconnectAttempts) {
				log.info(() -> "Connecting to broker: " + broker);
				countConnectionAttempts += 1;
				connectionInProgress = true;
				try {
					mqttClient.connect(connOpts);
				} catch (MqttSecurityException e) {
					connectionInProgress = false;
					log.severe(ExceptionUtils.getStackTrace(e));
				} catch (MqttException e) {
					connectionInProgress = false;
					log.severe(ExceptionUtils.getStackTrace(e));
				}
			} else {
				log.warning("Maximum number of connection attempts reached.");
			}
		}
	}
	
	public void disconnect() {
		try {
			if (mqttClient != null && mqttClient.isConnected()) {
				mqttClient.disconnect();
				SimulationExecutionListenerMQTTCameoPlugin.setConnectionStatusText(SimulationExecutionListenerMQTTCameoPluginConstants.DISCONNECTED);
				countConnectionAttempts = 0;
			}
		} catch (MqttException e) {
			log.severe(ExceptionUtils.getStackTrace(e));
		}
	}

	public void sendMessage(String topic, String content) {
		sendMessage(topic, content, false);
	}
	
	public void sendMessage(String topic, String content, boolean retained) {
		log.fine("sendMessage:begin");
		if (!mqttClient.isConnected()) {
			return;
		}
		MqttMessage message = new MqttMessage(content.getBytes());
		message.setQos(qos);
		message.setRetained(retained);
		try {
			mqttClient.publish(topic, message);
		} catch (MqttPersistenceException e) {
			log.severe(ExceptionUtils.getStackTrace(e));
		} catch (MqttException e) {
			log.severe(ExceptionUtils.getStackTrace(e));
		}
		log.fine("sendMessage:end");
	}

	private void initClient() {
		log.fine("initClient:begin");
		try {
			if (mqttClient != null && mqttClient.isConnected()) {
				mqttClient.disconnect();
			}
			countConnectionAttempts = 0;
			connectionInProgress = false;
			mqttClient = new MqttAsyncClient(broker, clientId, persistence);
			mqttClient.setCallback(new MqttCallback() {

				@Override
				public void authPacketArrived(int arg0, MqttProperties arg1) {
					// not implemented
					
				}

				@Override
				public void connectComplete(boolean arg0, String arg1) {
					log.info(() -> "Connection established: " + arg1);
					SimulationExecutionListenerMQTTCameoPlugin.setConnectionStatusText(SimulationExecutionListenerMQTTCameoPluginConstants.CONNECTED);
					countConnectionAttempts = 0;
					connectionInProgress = false;
					registerAllSubscribers();
				}

				@Override
				public void deliveryComplete(IMqttToken arg0) {
					// not implemented
					
				}

				@Override
				public void disconnected(MqttDisconnectResponse arg0) {
					log.severe(() -> "Client got disconnected: " + arg0);
					SimulationExecutionListenerMQTTCameoPlugin.setConnectionStatusText(SimulationExecutionListenerMQTTCameoPluginConstants.DISCONNECTED);
					connect();
				}

				@Override
				public void messageArrived(String topic, MqttMessage message) throws Exception {
					JSONObject payload = new JSONObject(new String(message.getPayload())); // NOSONAR
					
					if (topic.equalsIgnoreCase(SimulationExecutionListenerMQTTCameoPluginConstants.MODEL_SIGNALS_CREATE_TOPIC)) {
						handleCreateSignalMessage(payload);
					} else if (topic.equalsIgnoreCase(SimulationExecutionListenerMQTTCameoPluginConstants.MODEL_VARIABLE_SET_TOPIC)) {
						handleSetVariableMessage(payload);
					} else {
						log.info(() -> "UNHANDLED TOPIC: " + topic);
					}
				}

				@Override
				public void mqttErrorOccurred(MqttException arg0) {
					log.severe(() -> "MQTT error occured: " + ExceptionUtils.getStackTrace(arg0));
				}
				
			});
			
			connOpts = new MqttConnectionOptions();
			connOpts.setAutomaticReconnect(true);
			connOpts.setCleanStart(false);
			connOpts.setConnectionTimeout(20);
			connOpts.setKeepAliveInterval(10);
			MqttMessage lastWillmessage = new MqttMessage(Boolean.FALSE.toString().getBytes());
			lastWillmessage.setQos(0);
			lastWillmessage.setRetained(true);
			connOpts.setWill(SimulationExecutionListenerMQTTCameoPluginConstants.MODEL_EXECUTION_ACTIVE_TOPIC, lastWillmessage);
			
			if (protocol.equalsIgnoreCase(SimulationExecutionListenerMQTTCameoPluginConstants.WSS)) {
				String caFileName = PropertyLoader.getInstance().getPropertyWithDefaultValue(SimulationExecutionListenerMQTTCameoPluginConstants.SECURITY_CAFILE_NAME, SimulationExecutionListenerMQTTCameoPluginConstants.DEFAULT_SECURITY_CAFILE_NAME);
				SSLSocketFactory socketFactory = SslUtils.getSingleSocketFactory(pathToPlugin + "\\" + caFileName);
				connOpts.setSocketFactory(socketFactory);
			}
			log.info(() -> "Connecting to broker: " + broker);	
		} catch (Exception e) {
			log.severe(() -> ExceptionUtils.getStackTrace(e));
		}
		log.fine("initClient:end");
	}
	
	private void registerSubscriber(String topic) {
		log.fine("registerSubscriber:begin");
		try {
			if (mqttClient.isConnected()) {
				mqttClient.subscribe(topic, qos);
				log.info(() -> "Registered subscriber on topic: " + topic);
			}
		} catch (Exception e) {
			log.severe(ExceptionUtils.getStackTrace(e));
		}
		log.fine("registerSubscriber:end");
	}
	
	private void registerAllSubscribers() {
		log.fine("registerAllSubscribers:begin");
		log.info("Registering all MQTT Subscribers");
		registerSubscriber(SimulationExecutionListenerMQTTCameoPluginConstants.MODEL_SIGNALS_CREATE_TOPIC);
		registerSubscriber(SimulationExecutionListenerMQTTCameoPluginConstants.MODEL_VARIABLE_SET_TOPIC);
		log.fine("registerAllSubscribers:end");
	}
	
	private void handleCreateSignalMessage(JSONObject payload) {
		log.fine("handleCreateSignalMessage:begin");
		if (checkPrerequisites(payload)) {
			var target = getTarget(payload);
			if (target != null) {
				var signalJSON = payload.getJSONObject(SimulationExecutionListenerMQTTCameoPluginConstants.SIGNAL);
				if (signalJSON != null) {
					var signalName = signalJSON.getString(SimulationExecutionListenerMQTTCameoPluginConstants.NAME);
					log.fine(() -> "Signalname: " + signalName);
					if (signalName != null) {
						createSpecifiedSignalAndSendToTarget(signalJSON, signalName, target);								
					} else {
						log.warning("Missing signal name");
					}
				} else {
					log.warning("Missing signal specification");
				}
			} else {
				log.warning("Target not found");
			}				
		}
		log.fine("handleCreateSignalMessage:end");
	}
	
	private void handleSetVariableMessage(JSONObject payload) {
		log.fine("handleSetVariableMessage:begin");
		if (checkPrerequisites(payload)) {
			var target = getTarget(payload);
			if (target != null) {
				var variableJSON = payload.getJSONObject(SimulationExecutionListenerMQTTCameoPluginConstants.VARIABLE);
				if (variableJSON != null) {
					var variableName = variableJSON.getString(SimulationExecutionListenerMQTTCameoPluginConstants.NAME);
					if (variableName != null) {
						log.fine(() -> "Variable name: " + variableName);
						if (variableJSON.has(SimulationExecutionListenerMQTTCameoPluginConstants.VALUE)) {							
							setVariableOfTargetToSpecifiedValue(variableJSON, variableName, target);

						} else {
							log.warning("Missing variable value");
						}
					} else {
						log.warning("Missing variable name");
					}
				} else {
					log.warning("Missing variable specification");
				}
			} else {
				log.warning("Target not found");
			}					
		}						
		log.fine("handleSetVariableMessage:end");
	}

	private void setVariableOfTargetToSpecifiedValue(JSONObject variableJSON, String variableName, Object_ target) {
		JSONObject json = new JSONObject();
		json.put(variableName, variableJSON.get(SimulationExecutionListenerMQTTCameoPluginConstants.VALUE));
		ArrayList<Object> results = new ArrayList<>();
		ClassifierHelper.collectInheritedAttributes(target.getFirstType(), results, true, true);
		if (!results.isEmpty()) {
			Property property = null;
			for (Object attribute_obj : results) {
				if (attribute_obj instanceof Property) {
					var attribute = (Property)attribute_obj;
					if (attribute.getName().equalsIgnoreCase(variableName)) {
						property = attribute;
						break;
					}
				}
			}
			if (property != null) {
				var message = "Property: " + property.getHumanName();
				log.fine(message);
				ModelUtils.mergeSpecifiedValueIntoFeatureValue(target, property, json);
			} else {
				log.warning(() -> "Target has no variable with name: " + variableName);
			}
		} else {
			log.warning("Target has no variables.");
		}
	}
	
	private Object_ getTarget(JSONObject payload) {
		var targetName = payload.getString(SimulationExecutionListenerMQTTCameoPluginConstants.TARGET);
		log.fine(() -> "Targetname: " + targetName);
		if (targetName != null) {
			return ModelUtils.findTarget(targetName, listener);
		} else {
			log.warning("Target not specified.");
		}	
		return null;

	}
	
	private boolean checkPrerequisites(JSONObject payload) {
		if (payload != null) {
			if (listener.isExecutionStarted()) {
				return true;
			} else {
				log.warning("Execution is not running.");
			}
		} else {
			log.warning("Missing payload.");
		}
		return false;
	}
	
	private void createSpecifiedSignalAndSendToTarget(JSONObject signalJSON, String signalName, Object_ target) {
		if (SimulationExecutionListenerMQTTCameoPlugin.getCameoModelConnector().getSignalsInModel().containsKey(signalName)) {
			var signal = SimulationExecutionListenerMQTTCameoPlugin.getCameoModelConnector().getSignalsInModel().get(signalName);
			var signalInstance = ModelUtils.createSignalInstance(signal, signalJSON.has(SimulationExecutionListenerMQTTCameoPluginConstants.PAYLOAD) ? signalJSON.getJSONObject(SimulationExecutionListenerMQTTCameoPluginConstants.PAYLOAD) : null);
			if (signalInstance != null) {
				log.info(() -> "Sending signal instance (" + signalInstance.toString()  + ") to target (" + fUMLHelper.buildStringFor(target) + ")");
				fUMLHelper.sendSignal(signalInstance, target);
			} else {
				log.severe("Error creating signal instance");
			}
		} else {
			log.warning("Signal not found");
		}
	}
}
