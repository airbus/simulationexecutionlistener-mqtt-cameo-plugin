/*
* Copyright (c) 2025 AIRBUS and its affiliates.
* This Source Code Form is subject to the terms of the Mozilla Public
* License, v. 2.0. If a copy of the MPL was not distributed with this
* file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

package com.airbus.mqttconnector;

import java.io.IOException;
import java.util.logging.Logger;

import org.apache.commons.lang3.exception.ExceptionUtils;

import com.airbus.mqttconnector.gui.ConnectorWindowComponentContent;
import com.airbus.mqttconnector.gui.ConnectorWindowsConfigurator;
import com.airbus.mqttconnector.utils.PropertyLoader;
import com.nomagic.magicdraw.plugins.Plugin;
import com.nomagic.magicdraw.simulation.SimulationManager;
import com.nomagic.magicdraw.ui.ProjectWindow;
import com.nomagic.magicdraw.ui.ProjectWindowsManager;

public class SimulationExecutionListenerMQTTCameoPlugin extends Plugin
{
	
	public static final Logger log = Logger.getLogger(SimulationExecutionListenerMQTTCameoPlugin.class.getName());
	
	private static ProjectWindow myProjectWindow = null;
	private static final CameoModelConnector modelConnector = new CameoModelConnector();
	private static final MQTTConnector mqttConnector = new MQTTConnector();
	
	@Override
	public void init()
	{
		log.info("SimulationExecutionListenerMQTTCameoPlugin initialized.");

		var pd = this.getDescriptor();
		var pathToPlugin = "";
		
		try {
			pathToPlugin = pd.getPluginDirectory().getCanonicalPath();
			PropertyLoader.getInstance().setPluginDirectory(pathToPlugin);
			var message = "Loading properties from path: " + pathToPlugin; 
			log.info(message);
			PropertyLoader.getInstance().loadAllProperties();
		} catch (IOException e) {
			log.severe(ExceptionUtils.getStackTrace(e));
		}
		
		if (PropertyLoader.getInstance().getPropertyWithDefaultValue(SimulationExecutionListenerMQTTCameoPluginConstants.PLUGIN_ENABLED, Boolean.TRUE.toString()).equalsIgnoreCase(Boolean.TRUE.toString())) {
			var listener = new CameoSimulationExecutionListener();
			
			if (PropertyLoader.getInstance().getPropertyWithDefaultValue(SimulationExecutionListenerMQTTCameoPluginConstants.PLUGIN_GUI, Boolean.TRUE.toString()).equalsIgnoreCase(Boolean.TRUE.toString())) {
				ProjectWindowsManager.ConfiguratorRegistry.addConfigurator(new ConnectorWindowsConfigurator());
			}
			
			mqttConnector.init(listener, pathToPlugin);
					
			listener.setMQTTConnector(mqttConnector);
			SimulationManager.registerSimulationExecutionListener(listener);
		} else {
			log.info(() -> "SimulationExecutionListenerMQTTCameoPlugin has been disabled. To enable it, set the config property '" + SimulationExecutionListenerMQTTCameoPluginConstants.PLUGIN_ENABLED + "' to true");
		}
	}

	@Override
	public boolean close()
	{
		log.info("SimulationExecutionListenerMQTTCameoPlugin closing.");
		mqttConnector.sendMessage(SimulationExecutionListenerMQTTCameoPluginConstants.MODEL_EXECUTION_ACTIVE_TOPIC, Boolean.FALSE.toString(), true);
		if (mqttConnector.isConnected()) {
			mqttConnector.disconnect();
		}
		return true;
	}

	@Override
	public boolean isSupported()
	{
		return true;
	}
	
	public static CameoModelConnector getCameoModelConnector() {
		return modelConnector;
	}
	
	public static MQTTConnector getMqttconnector() {
		return mqttConnector;
	}

	public static void setMyProjectWindow(ProjectWindow projectWindow) {
		myProjectWindow = projectWindow;
	}
	
	public static void setConnectionStatusText(String text) {
		if (myProjectWindow != null) {
			ConnectorWindowComponentContent content = (ConnectorWindowComponentContent)myProjectWindow.getContent();
			content.getPanel().setClientStatusTextFieldText(text);
		}
	}

}
