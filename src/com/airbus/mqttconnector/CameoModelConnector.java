/*
* Copyright (c) 2025 AIRBUS and its affiliates.
* This Source Code Form is subject to the terms of the Mozilla Public
* License, v. 2.0. If a copy of the MPL was not distributed with this
* file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

package com.airbus.mqttconnector;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import com.nomagic.magicdraw.core.Application;
import com.nomagic.magicdraw.core.Project;
import com.nomagic.magicdraw.core.project.ProjectEventListener;
import com.nomagic.magicdraw.uml.Finder;
import com.nomagic.uml2.ext.magicdraw.commonbehaviors.mdcommunications.Signal;

public class CameoModelConnector {

	public static final Logger log = Logger.getLogger(CameoModelConnector.class.getName());
	
	private HashMap<String, Signal> signalsInModel = new HashMap<>();
	
	public CameoModelConnector() {
		registerListeners();
	}
	
	public void registerListeners() {
		Application.getInstance().addProjectEventListener(new ProjectEventListener() {
			
			@Override
			public void projectSaved(Project project, boolean arg1) {
				updateSignalsInModel(project);
			}
			
			@Override
			public void projectReplaced(Project arg0, Project arg1) {
				// Not implemented
				
			}
			
			@Override
			public void projectPreSaved(Project arg0, boolean arg1) {
				// Not implemented
				
			}
			
			@Override
			public void projectPreDeActivated(Project arg0) {
				// Not implemented
				
			}
			
			@Override
			public void projectPreClosedFinal(Project arg0) {
				// Not implemented
				
			}
			
			@Override
			public void projectPreClosed(Project arg0) {
				// Not implemented
				
			}
			
			@Override
			public void projectPreActivated(Project arg0) {
				// Not implemented
				
			}
			
			@Override
			public void projectOpenedFromGUI(Project arg0) {
				// Not implemented
				
			}
			
			@Override
			public void projectOpened(Project project) {
				updateSignalsInModel(project);
			}
			
			@Override
			public void projectDeActivated(Project arg0) {
				// Not implemented
				
			}
			
			@Override
			public void projectCreated(Project arg0) {
				// Not implemented
				
			}
			
			@Override
			public void projectClosed(Project arg0) {
				// Not implemented
				
			}
			
			@Override
			public void projectActivatedFromGUI(Project arg0) {
				// Not implemented
				
			}
			
			@Override
			public void projectActivated(Project arg0) {
				// Not implemented
				
			}
		});
	}
	
	public void updateSignalsInModel(Project project) {
		signalsInModel.clear();
		final Collection<Signal> existingSignals = Finder.byTypeRecursively().find(project, new java.lang.Class[]{Signal.class});
		var sb = new StringBuilder();
		sb.append("\nAll signals in model: ");
		existingSignals.forEach(signal -> {
			sb.append("\n\t" + signal.getName());
			sb.append("\n\t" + signal.getQualifiedName());
			signalsInModel.put(signal.getName(), signal);
		});
		log.info(sb::toString);
	}
	
	public Map<String, Signal> getSignalsInModel() {
		return signalsInModel;
	}
}
