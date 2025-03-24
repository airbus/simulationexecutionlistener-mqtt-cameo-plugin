/*
* Copyright (c) 2025 AIRBUS and its affiliates.
* This Source Code Form is subject to the terms of the Mozilla Public
* License, v. 2.0. If a copy of the MPL was not distributed with this
* file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

package com.airbus.mqttconnector.utils;

import java.math.BigDecimal;
import java.util.Collection;
import java.util.List;
import java.util.logging.Logger;

import org.json.JSONArray;
import org.json.JSONObject;

import com.airbus.mqttconnector.CameoSimulationExecutionListener;
import com.airbus.mqttconnector.SimulationExecutionListenerMQTTCameoPluginConstants;
import com.nomagic.magicdraw.core.Application;
import com.nomagic.magicdraw.simulation.fuml.fUMLHelper;
import com.nomagic.magicdraw.uml.Finder;
import com.nomagic.uml2.ext.jmi.helpers.ModelHelper;
import com.nomagic.uml2.ext.magicdraw.classes.mdkernel.DataType;
import com.nomagic.uml2.ext.magicdraw.classes.mdkernel.Property;
import com.nomagic.uml2.ext.magicdraw.classes.mdkernel.Type;
import com.nomagic.uml2.ext.magicdraw.commonbehaviors.mdcommunications.Signal;

import fUML.Semantics.Classes.Kernel.BooleanValue;
import fUML.Semantics.Classes.Kernel.EnumerationValue;
import fUML.Semantics.Classes.Kernel.FeatureValue;
import fUML.Semantics.Classes.Kernel.FeatureValueList;
import fUML.Semantics.Classes.Kernel.IntegerValue;
import fUML.Semantics.Classes.Kernel.Object_;
import fUML.Semantics.Classes.Kernel.RealValue;
import fUML.Semantics.Classes.Kernel.StringValue;
import fUML.Semantics.Classes.Kernel.StructuredValue;
import fUML.Semantics.Classes.Kernel.UnlimitedNaturalValue;
import fUML.Semantics.Classes.Kernel.Value;
import fUML.Semantics.Classes.Kernel.ValueList;
import fUML.Semantics.CommonBehaviors.Communications.SignalInstance;

public class ModelUtils {

	public static final Logger log = Logger.getLogger(ModelUtils.class.getName());
	
	private ModelUtils() {
		
	}
	
	public static void checkTypeCompatilityWithPrimitiveType(Property property, String primitiveType) {
		@SuppressWarnings("rawtypes")
		java.lang.Class type = null;
		Collection<Type> typeObjects = Finder.byNameAllRecursively().find(Application.getInstance().getProject(), type, primitiveType);
		log.fine(typeObjects.toString());
		for (Type typeObject : typeObjects) {
		    log.fine(typeObject.getHumanName());
			boolean valueType = false;
		    for (var stereotype : typeObject.getAppliedStereotype()) {
		    	valueType = valueType || stereotype.getName().equalsIgnoreCase("valueType");
		    }
			if (valueType) {
				var message = "Checking compatiblity of type " + property.getType().getHumanName() + " and " + typeObject.getHumanName();
				log.fine(message);
				var compatible = ModelHelper.isSecondTypeCompatibleToFirst(typeObject, property.getType(), false);
		        if (!compatible) {
		        	var warning = "Property type is of type " + property.getType().getHumanName() + " but specified value is of type " + primitiveType + "."; 
		        	log.warning(warning);
		        }		
		    }
		}
	}
	
	public static void mergeSpecifiedValuesIntoFeatureValues(StructuredValue object, List<Property> properties, JSONObject specification) {
		for (var property : properties) {
			mergeSpecifiedValueIntoFeatureValue(object, property, specification);
		}
	}
	
	public static SignalInstance createSignalInstance(Signal signal, JSONObject specification) { 
		var signalInstance = new SignalInstance();
		signalInstance.type = signal;
		
		if (signal.getOwnedAttribute() != null && signal.getOwnedAttribute().size() > 0) {
			mergeSpecifiedValuesIntoFeatureValues(signalInstance, signal.getAttribute(), specification);
		}
		return signalInstance;
	}
	
	public static Object_ findTarget(String targetName, CameoSimulationExecutionListener listener) {
		Object_ target = null;
		if (targetName.contains(".")) {
			String[] splitTargetName = targetName.split("[.]");
			if (listener.getObjects().containsKey(splitTargetName[0])) {
				Object_ owner = listener.getObjects().get(splitTargetName[0]);
				target = findTargetFromFeatureValuesRecursive(targetName.substring(targetName.indexOf(".") + 1), owner.getFeatureValues());
			}
		} else {
			if (listener.getObjects().containsKey(targetName)) {
				target = listener.getObjects().get(targetName);
			}
		}
		return target;
	}
	
	public static void mergeSpecifiedValueIntoFeatureValue(StructuredValue object, Property property, JSONObject specification) {
		var propertyName = property.getName();
		if (specification != null && specification.has(propertyName)) {
			var specifiedValue = specification.get(propertyName);
			var value = getObjectFromSpecifiedValue(property, specifiedValue);
            if (value != null) {
            	var message = "Setting property " + propertyName + " to Value " + value.toString();
            	log.info(message);
            	fUMLHelper.setFeatureValue(object, propertyName, value);
            }
		} else {
			if (property.getDefaultValue() != null) {
				var message = "Setting property " + propertyName + " to default Value " + property.getDefaultValue(); 
				log.info(message);
				fUMLHelper.setFeatureValue(object, propertyName, property.getDefaultValue());
			}
		}
	}

	private static Object getObjectFromSpecifiedValue(Property property, Object specifiedValue) {
		Object value = null;
		if (specifiedValue instanceof Integer || specifiedValue instanceof Long) {
		    int intToUse = ((Number)specifiedValue).intValue();
		    value = intToUse;
		    ModelUtils.checkTypeCompatilityWithPrimitiveType(property, "Integer");	
		} else if (specifiedValue instanceof Boolean) {
		    boolean boolToUse = ((Boolean)specifiedValue).booleanValue();
		    value = boolToUse;
		} else if (specifiedValue instanceof Float || specifiedValue instanceof Double || specifiedValue instanceof BigDecimal) {
		    double floatToUse = ((Number)specifiedValue).doubleValue();
		    value = floatToUse;
		    ModelUtils.checkTypeCompatilityWithPrimitiveType(property, "Real");
		} else if (specifiedValue instanceof String) {
		    String stringToUse = (String)specifiedValue;
		    value = stringToUse;
		} else if (specifiedValue instanceof JSONObject){
			value = mergeObjectIntoFeatureValue(property, specifiedValue);
		} else {
			log.warning("Not supported.");
		}
		return value;
	}

	private static Object mergeObjectIntoFeatureValue(Property property, Object v) {
		var typeObject = property.getType();
		if (typeObject != null) {
			var stereotypesList = new JSONArray();
			typeObject.getAppliedStereotype().forEach(stereotype->stereotypesList.put(stereotype.getName()));
			if (typeObject.getClassType() == DataType.class) {
				var dataTypeObject = (DataType)typeObject;
				if (dataTypeObject.hasAttribute()) {
					var dataValueInstance = new fUML.Semantics.Classes.Kernel.DataValue(dataTypeObject);
					dataValueInstance.type = dataTypeObject;
					mergeSpecifiedValuesIntoFeatureValues(dataValueInstance, dataTypeObject.getAttribute(), (JSONObject)v);
					return dataValueInstance;
				}
			}
		}
		return null;
	}
	
	private static Object_ findTargetFromFeatureValuesRecursive(String targetName, FeatureValueList featureValues) {
		Object_ target = null;
		if (targetName.contains(".")) {
			Object_ owner = findTargetFromFeatureValuesRecursive(targetName.split("[.]")[0], featureValues);
			return findTargetFromFeatureValuesRecursive(targetName.substring(targetName.indexOf(".") + 1), owner.getFeatureValues());
		} else {
			for (FeatureValue featureValue : featureValues) {
				if (featureValue.getElement().getName().equalsIgnoreCase(targetName)) {
					target = (Object_)fUMLHelper.getObjectFromFeatureValue(featureValue);
				}
			}
		}
		return target;
	}	
	
    private static void putValueIntoJSONArray(JSONArray array, Object value) {
    	if (value instanceof IntegerValue) {
    		array.put(((IntegerValue)value).getValue());
    	} else if (value instanceof RealValue) {        		
    		array.put(((RealValue)value).getValue());
    	} else if (value instanceof StringValue) {
    		array.put(((StringValue)value).getValue());
    	} else if (value instanceof UnlimitedNaturalValue) {
    		array.put(((UnlimitedNaturalValue)value).getValue());    		
    	} else if (value instanceof EnumerationValue) {
    		array.put(((EnumerationValue)value).toString());      
    	} else if (value instanceof BooleanValue) {
    		array.put(((BooleanValue)value).getValue()); 
    	} else if (value instanceof Double || value instanceof Integer || value instanceof String) {
    		array.put(value);
    	} else {
    		array.put(fUMLHelper.buildStringFor(value));
    	}
    }
    
    public static void putValueListIntoJSONObject(JSONObject message, ValueList valueList) {
		if (valueList.size() > 1) {
			JSONArray array = new JSONArray();
    		for (Value valueElement : valueList) {
    			putValueIntoJSONArray(array, valueElement);
    		}
    		message.put(SimulationExecutionListenerMQTTCameoPluginConstants.VALUE, array);
		} else if (valueList.size() == 1) {
			putValueIntoJSONObject(message, valueList.get(0));
		}
    }
    
    public static void putValueIntoJSONObject(JSONObject message, Object newValue) {
    	if (newValue != null) {
        	if (newValue instanceof ValueList) {
        		putValueListIntoJSONObject(message, (ValueList) newValue);
        	} else if (newValue instanceof IntegerValue) {
        		message.put(SimulationExecutionListenerMQTTCameoPluginConstants.VALUE, ((IntegerValue)newValue).getValue());
        	} else if (newValue instanceof RealValue) {        		
        		message.put(SimulationExecutionListenerMQTTCameoPluginConstants.VALUE, ((RealValue)newValue).getValue());
        	} else if (newValue instanceof StringValue) {
        		message.put(SimulationExecutionListenerMQTTCameoPluginConstants.VALUE, ((StringValue)newValue).getValue());
        	} else if (newValue instanceof UnlimitedNaturalValue) {
        		message.put(SimulationExecutionListenerMQTTCameoPluginConstants.VALUE, ((UnlimitedNaturalValue)newValue).getValue());        		
        	} else if (newValue instanceof EnumerationValue) {
        		message.put(SimulationExecutionListenerMQTTCameoPluginConstants.VALUE, ((EnumerationValue)newValue).toString());
        	} else if (newValue instanceof BooleanValue) {
        		message.put(SimulationExecutionListenerMQTTCameoPluginConstants.VALUE, ((BooleanValue)newValue).getValue());        		
        	} else if (newValue instanceof Double || newValue instanceof Integer || newValue instanceof String) {
        		message.put(SimulationExecutionListenerMQTTCameoPluginConstants.VALUE, newValue);      		
        	} else {
        		message.put(SimulationExecutionListenerMQTTCameoPluginConstants.VALUE, fUMLHelper.buildStringFor(newValue));
        	}
    	} else {
    		message.put(SimulationExecutionListenerMQTTCameoPluginConstants.VALUE, SimulationExecutionListenerMQTTCameoPluginConstants.NA);
    	}
    }	
}
