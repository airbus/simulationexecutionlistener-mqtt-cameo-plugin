/*
* Copyright (c) 2025 AIRBUS and its affiliates.
* This Source Code Form is subject to the terms of the Mozilla Public
* License, v. 2.0. If a copy of the MPL was not distributed with this
* file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

package com.airbus.mqttconnector.utils;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Logger;
import java.util.stream.Stream;

import org.apache.commons.lang3.exception.ExceptionUtils;

import com.airbus.mqttconnector.SimulationExecutionListenerMQTTCameoPluginConstants;

public class PropertyLoader {
	
	public static final Logger log = Logger.getLogger(PropertyLoader.class.getName());
	
	private Properties allProperties = null;
	private String pluginDirectory = null;
	
	private static PropertyLoader instance;
	
	private PropertyLoader() {

	}
	
	public static PropertyLoader getInstance() {
		if (instance == null) {
			instance = new PropertyLoader();
		}
		return PropertyLoader.instance;
	}
	
	public void loadAllProperties() {
		allProperties = mergeProperties(loadProperties(SimulationExecutionListenerMQTTCameoPluginConstants.PLUGIN_CONFIG));
	}
	
	public void setPluginDirectory(String path) {
		this.pluginDirectory = path;
	}
	
	public int getIntPropertyWithDefaultValue(String property, int defaultValue) {
		try {
			return Integer.parseInt(allProperties.getProperty(property));
		} catch (Exception e){
			return defaultValue;
		}
	}
	
	public String getPropertyWithDefaultValue(String property, String defaultValue) {
		if (allProperties != null && allProperties.getProperty(property) != null)
			return allProperties.getProperty(property);
		return defaultValue;
	}
	
	public String getProperty(String property) {
		return allProperties.getProperty(property);
	}
	
	private Properties loadProperties(String propertyFileName) {
		var message = "Loading: " + propertyFileName; 
		log.info(message);
		var properties = new Properties();
		try (InputStream input = new FileInputStream(pluginDirectory + "/" + propertyFileName) ){
			log.info("Found properties file. Parsing properties");
			properties.load(input);
		} catch (IOException e) {
			log.severe("Properties not found");
			log.severe(ExceptionUtils.getStackTrace(e));
		}
		return properties;
	}
	
	private static Properties mergeProperties(Properties... properties) {
	    return Stream.of(properties)
	        .collect(Properties::new, Map::putAll, Map::putAll);
	}	
	
}
