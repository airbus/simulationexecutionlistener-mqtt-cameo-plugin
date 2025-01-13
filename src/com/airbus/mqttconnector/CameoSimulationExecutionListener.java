/*
* Copyright (c) 2025 AIRBUS and its affiliates.
* This Source Code Form is subject to the terms of the Mozilla Public
* License, v. 2.0. If a copy of the MPL was not distributed with this
* file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

package com.airbus.mqttconnector;

import java.util.Collection;
import java.util.HashMap;
import java.util.logging.Logger;

import org.json.JSONArray;
import org.json.JSONObject;

import com.airbus.mqttconnector.utils.ModelUtils;
import com.nomagic.magicdraw.core.Application;
import com.nomagic.magicdraw.simulation.execution.SimulationExecution;
import com.nomagic.magicdraw.simulation.execution.SimulationExecutionListener;
import com.nomagic.magicdraw.simulation.fuml.fUMLHelper;
import com.nomagic.magicdraw.uml.Finder;
import com.nomagic.uml2.ext.magicdraw.classes.mdkernel.Element;
import com.nomagic.uml2.ext.magicdraw.commonbehaviors.mdbasicbehaviors.Behavior;
import com.nomagic.uml2.ext.magicdraw.statemachines.mdbehaviorstatemachines.State;

import fUML.Semantics.Classes.Kernel.FeatureValue;
import fUML.Semantics.Classes.Kernel.Object_;
import fUML.Semantics.Classes.Kernel.StructuredValue;
import fUML.Semantics.CommonBehaviors.BasicBehaviors.ParameterValueList;
import fUML.Semantics.CommonBehaviors.Communications.SignalInstance;

public class CameoSimulationExecutionListener extends SimulationExecutionListener {

	public static final Logger log = Logger.getLogger(CameoSimulationExecutionListener.class.getName());
	
	private MQTTConnector mqttConnector;
	private boolean isExecutionActive = false;
	private HashMap<String, Object_> objects = new HashMap<>();
	
	public HashMap<String, Object_> getObjects() {
		return objects;
	}
	
	public void setMQTTConnector(MQTTConnector connector) {
		this.mqttConnector = connector;
	}
	
	public boolean isExecutionStarted() {
		return isExecutionActive;
	}
	
	@Override
    public void executionStarted(SimulationExecution execution) {
    	log.info("executionStarted");
    	this.isExecutionActive = true;
    	mqttConnector.sendMessage(SimulationExecutionListenerMQTTCameoPluginConstants.MODEL_EXECUTION_ACTIVE_TOPIC, Boolean.TRUE.toString(), true);
    }
    
    @Override
	public void beforeContextInitialized(SimulationExecution arg0) {
		super.beforeContextInitialized(arg0);
	}

	@Override
	public void beforeObjectDestroyed(Object_ arg0) {
		super.beforeObjectDestroyed(arg0);
	}

	@Override
	public void contextInitialized(SimulationExecution arg0) {
		super.contextInitialized(arg0);
	}

	@Override
	public void objectStateActivated(StructuredValue object, State state) {
		super.objectStateActivated(object, state);
		
		JSONObject message = new JSONObject();
		message.put(SimulationExecutionListenerMQTTCameoPluginConstants.OBJECT, fUMLHelper.buildStringFor(object));
		message.put(SimulationExecutionListenerMQTTCameoPluginConstants.STATE, state.getQualifiedName());
		mqttConnector.sendMessage(SimulationExecutionListenerMQTTCameoPluginConstants.MODEL_STATE_ACTIVATED_TOPIC, message.toString(2));
	}

	@Override
    public void elementActivated(Element element, Collection<?> values) {
		// not implemented
    }
    
    @Override
    public void elementDeactivated(Element element, Collection<?> values) {
		if (element instanceof State) {
			var state = (State)element;
			
			JSONObject message = new JSONObject();
			message.put(SimulationExecutionListenerMQTTCameoPluginConstants.STATE, state.getQualifiedName());
			mqttConnector.sendMessage(SimulationExecutionListenerMQTTCameoPluginConstants.MODEL_STATE_DEACTIVATED_TOPIC, message.toString(2));
		}
    }
    
    @Override
    public void eventTriggered(SignalInstance signal) {
    	//not implemented
    }
    
    @Override
    public void behaviorCalled(Behavior behavior, ParameterValueList pvl, Object_ caller, Object_ target, boolean isSynchronous) {
    	//not implemented
    }
    
    @Override
    public void objectCreated(Object_ sender, Object_ object) {
    	if (object != null) {
   		
    		JSONObject metaClass = new JSONObject();
    		if (object.getFirstType()!= null) {
	    		Element typeObject = Finder.byQualifiedName().find(Application.getInstance().getProject(), object.getFirstType().getQualifiedName());
	    		if (typeObject != null) {
		    		var stereotypesList = new JSONArray();
		    		typeObject.getAppliedStereotype().forEach(stereotype->stereotypesList.put(stereotype.getName()));
		    		metaClass.put(SimulationExecutionListenerMQTTCameoPluginConstants.STEREOTYPES, stereotypesList);
		    		metaClass.put(SimulationExecutionListenerMQTTCameoPluginConstants.FULL_NAME, typeObject.getClassType().getName());
		    		metaClass.put(SimulationExecutionListenerMQTTCameoPluginConstants.SIMPLE_NAME, typeObject.getClassType().getSimpleName());
	    		}
    		}
    		
    		if (!object.getClass().getName().equalsIgnoreCase(SimulationExecutionListenerMQTTCameoPluginConstants.CUSTOMIZATION_CLASS_NAME)) {
	    		JSONObject message = new JSONObject();
	    		message.put(SimulationExecutionListenerMQTTCameoPluginConstants.ID, fUMLHelper.buildStringFor(object));
	    		JSONObject type = new JSONObject();
	    		type.put(SimulationExecutionListenerMQTTCameoPluginConstants.NAME, object.getFirstType().getName());
	    		type.put(SimulationExecutionListenerMQTTCameoPluginConstants.QUALIFIED_NAME, object.getFirstType().getQualifiedName());
	    		type.put(SimulationExecutionListenerMQTTCameoPluginConstants.METACLASS2, metaClass);
	    		message.put(SimulationExecutionListenerMQTTCameoPluginConstants.TYPE, type);
	    		
	    		mqttConnector.sendMessage(SimulationExecutionListenerMQTTCameoPluginConstants.MODEL_OBJECTS_TOPIC, message.toString(2));
	    		objects.put(object.getFirstType().getName(), object);
    		}
    	}
    }
    
    @Override
    public void valueChange(StructuredValue context, FeatureValue featureValue, Object oldValue, Object newValue) {
    	var objectString = SimulationExecutionListenerMQTTCameoPluginConstants.NA;
    	var propertyString = SimulationExecutionListenerMQTTCameoPluginConstants.NA;
    	
    	if (context != null) {
    		objectString = fUMLHelper.buildStringFor(context);
    	}
    	if (featureValue != null && featureValue.getMatchingFeature() != null) {
   			propertyString = featureValue.getMatchingFeature().getQualifiedName();
    	}

		JSONObject message = new JSONObject();
		message.put(SimulationExecutionListenerMQTTCameoPluginConstants.OBJECT, objectString);
		message.put(SimulationExecutionListenerMQTTCameoPluginConstants.PROPERTY, propertyString);
		ModelUtils.putValueIntoJSONObject(message, newValue);
		mqttConnector.sendMessage(SimulationExecutionListenerMQTTCameoPluginConstants.MODEL_VARIABLE_VALUE_TOPIC, message.toString(2));
    }
    
    @Override
    public void executionTerminated(SimulationExecution execution) {
    	log.info("executionTerminated");
    	this.isExecutionActive = false;
    	mqttConnector.sendMessage(SimulationExecutionListenerMQTTCameoPluginConstants.MODEL_EXECUTION_ACTIVE_TOPIC, Boolean.FALSE.toString(), true);
    }
    
    @Override
    public void configLoaded(Element config, SimulationExecution execution) {
    	//not implemented
    }
    
    @Override
    public void busyStatusChange(StructuredValue context, Object oldValue, Object newValue) {
    	//not implemented
    }
}
