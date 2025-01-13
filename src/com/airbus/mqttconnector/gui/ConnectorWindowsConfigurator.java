/*
* Copyright (c) 2025 AIRBUS and its affiliates.
* This Source Code Form is subject to the terms of the Mozilla Public
* License, v. 2.0. If a copy of the MPL was not distributed with this
* file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

package com.airbus.mqttconnector.gui;

import java.util.logging.Logger;

import javax.swing.Icon;

import com.airbus.mqttconnector.SimulationExecutionListenerMQTTCameoPlugin;
import com.nomagic.magicdraw.core.Project;
import com.nomagic.magicdraw.ui.ProjectWindow;
import com.nomagic.magicdraw.ui.ProjectWindowsConfigurator;
import com.nomagic.magicdraw.ui.ProjectWindowsManager;
import com.nomagic.magicdraw.ui.WindowComponentInfo;
import com.nomagic.magicdraw.ui.WindowsManager;
import com.nomagic.ui.ScalableImageIcon;

public class ConnectorWindowsConfigurator implements ProjectWindowsConfigurator
{

	public static final Logger log = Logger.getLogger(ConnectorWindowsConfigurator.class.getName());
	
	public static final String ID = "CUSTOM_PROJECT_WINDOW_PANEL_ID";

	public static final Icon WINDOW_ICON = new ScalableImageIcon(ConnectorWindowsConfigurator.class, "custom_window.png");


	@Override
	public void configure(Project project, ProjectWindowsManager projectWindowsManager) {
		addProjectWindow(project, projectWindowsManager);
	}

	public static ProjectWindow addProjectWindow(Project project, ProjectWindowsManager projectWindowsManager) {
		log.info("Adding project window.");
		ProjectWindow projectWindow = new ProjectWindow(createWindowComponentInfo(), new ConnectorWindowComponentContent());
		projectWindowsManager.addWindowWithoutOpen(project, projectWindow);
		projectWindowsManager.activateWindow(project, projectWindow.getId());
		SimulationExecutionListenerMQTTCameoPlugin.setMyProjectWindow(projectWindow);
		
		return projectWindow;
	}

	private static WindowComponentInfo createWindowComponentInfo() {
		return new WindowComponentInfo(ID,
									   "MQTT Connector",
									   WINDOW_ICON,
									   WindowsManager.SIDE_EAST,
									   WindowsManager.STATE_DOCKED, false);
	}
}
