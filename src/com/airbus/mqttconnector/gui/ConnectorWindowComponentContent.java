/*
* Copyright (c) 2025 AIRBUS and its affiliates.
* This Source Code Form is subject to the terms of the Mozilla Public
* License, v. 2.0. If a copy of the MPL was not distributed with this
* file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

package com.airbus.mqttconnector.gui;

import java.awt.Component;

import com.nomagic.magicdraw.ui.browser.WindowComponentContent;

public class ConnectorWindowComponentContent implements WindowComponentContent {
	private ConnectorPanel mPanel;

	public ConnectorWindowComponentContent() {
		mPanel = new ConnectorPanel();
	}

	@Override
	public Component getWindowComponent() {
		return mPanel;
	}

	@Override
	public Component getDefaultFocusComponent() {
		return null;
	}
	
	public ConnectorPanel getPanel() {
		return (ConnectorPanel)getWindowComponent();
	}
}