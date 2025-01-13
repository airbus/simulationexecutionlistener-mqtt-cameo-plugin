/*
* Copyright (c) 2025 AIRBUS and its affiliates.
* This Source Code Form is subject to the terms of the Mozilla Public
* License, v. 2.0. If a copy of the MPL was not distributed with this
* file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

package com.airbus.mqttconnector.gui;

import java.awt.Component;
import java.awt.Dimension;
import java.util.logging.Logger;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import com.airbus.mqttconnector.SimulationExecutionListenerMQTTCameoPlugin;
import com.airbus.mqttconnector.SimulationExecutionListenerMQTTCameoPluginConstants;
import com.airbus.mqttconnector.utils.PropertyLoader;

public class ConnectorPanel extends JPanel {

	public static final Logger log = Logger.getLogger(ConnectorPanel.class.getName());
	
	private static final String BROKER = "Broker:";
	private static final String CLIENT_STATUS = "Client Status:";
	private static final String DISCONNECT = "Disconnect";
	private static final String CONNECT = "Connect";
	
	private static final long serialVersionUID = 1749638888557745361L;
	private JTextField tfClientStatus;
	private JTextField tfBroker;
	private JButton btnConnect;
	private JButton btnDisconnect;

	public ConnectorPanel() {
		JPanel pnlClientStatus = createClientStatusPanel();
		JPanel pnlBrokertatus = createBrokerStatusPanel();
		JPanel pnlButton = createButtonPanel();
		
		//set layout and add all the panels
		setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));

		pnlClientStatus.setMaximumSize(new Dimension(300, (int) pnlClientStatus.getPreferredSize().getHeight()));
		pnlClientStatus.setAlignmentX(Component.LEFT_ALIGNMENT);
		pnlBrokertatus.setMaximumSize(new Dimension(300, (int) pnlBrokertatus.getPreferredSize().getHeight()));
		pnlBrokertatus.setAlignmentX(Component.LEFT_ALIGNMENT);
		pnlButton.setMaximumSize(new Dimension(300, (int) pnlButton.getPreferredSize().getHeight()));
		pnlButton.setAlignmentX(Component.LEFT_ALIGNMENT);
		
		add(Box.createRigidArea(new Dimension(0, 10)));
		add(pnlClientStatus);
		add(Box.createRigidArea(new Dimension(0, 10)));
		add(pnlBrokertatus);
		add(Box.createRigidArea(new Dimension(0, 10)));
		add(pnlButton);
		add(Box.createVerticalGlue());
		
		setPreferredSize(new Dimension(320, (int) getMinimumSize().getHeight()));
	}

	private JPanel createButtonPanel() {
		JPanel pnlButton = new JPanel();
		pnlButton.setLayout(new BoxLayout(pnlButton, BoxLayout.LINE_AXIS));
		
		pnlButton.add(Box.createRigidArea(new Dimension(5, 0)));
		
		btnConnect = new JButton(CONNECT);
		btnConnect.addActionListener(e -> SimulationExecutionListenerMQTTCameoPlugin.getMqttconnector().connectwithNewBroker(tfBroker.getText()));
		pnlButton.add(btnConnect);
		
		pnlButton.add(Box.createRigidArea(new Dimension(10, 0)));
		
		btnDisconnect = new JButton(DISCONNECT);
		btnDisconnect.addActionListener(e -> SimulationExecutionListenerMQTTCameoPlugin.getMqttconnector().disconnect());
		pnlButton.add(btnDisconnect);
		
		if (SimulationExecutionListenerMQTTCameoPlugin.getMqttconnector().isConnected()) {
			setClientStatusTextFieldText(SimulationExecutionListenerMQTTCameoPluginConstants.CONNECTED);
		} else {
			setClientStatusTextFieldText(SimulationExecutionListenerMQTTCameoPluginConstants.DISCONNECTED);
		}
		return pnlButton;
	}

	private JPanel createBrokerStatusPanel() {
		JPanel pnlBrokertatus = new JPanel();
		pnlBrokertatus.setLayout(new BoxLayout(pnlBrokertatus, BoxLayout.LINE_AXIS));
		
		pnlBrokertatus.add(Box.createRigidArea(new Dimension(5, 0)));
		
		JLabel lblBroker = new JLabel(BROKER);
		pnlBrokertatus.add(lblBroker);
		
		pnlBrokertatus.add(Box.createRigidArea(new Dimension(5, 0)));
		
		tfBroker = new JTextField();
		tfBroker.setEditable(true);
		tfBroker.setColumns(25);
		tfBroker.setText(PropertyLoader.getInstance().getPropertyWithDefaultValue(SimulationExecutionListenerMQTTCameoPluginConstants.BROKER_NAME, SimulationExecutionListenerMQTTCameoPluginConstants.DEFAULT_BROKER_NAME));
		
		pnlBrokertatus.add(tfBroker);
		return pnlBrokertatus;
	}

	private JPanel createClientStatusPanel() {
		JPanel pnlClientStatus = new JPanel();
		pnlClientStatus.setLayout(new BoxLayout(pnlClientStatus, BoxLayout.LINE_AXIS));
		
		pnlClientStatus.add(Box.createRigidArea(new Dimension(5, 0)));
		
		JLabel lblClientStatus = new JLabel(CLIENT_STATUS);
		pnlClientStatus.add(lblClientStatus);
		
		pnlClientStatus.add(Box.createRigidArea(new Dimension(5, 0)));
		
		tfClientStatus = new JTextField();
		tfClientStatus.setEditable(false);
		tfClientStatus.setColumns(25);
		
		pnlClientStatus.add(tfClientStatus);
		return pnlClientStatus;
	}
	
	public void setBrokerTextFieldText(String text) {
		tfBroker.setText(text);
	}
	
	public void setClientStatusTextFieldText(String text) {
		tfClientStatus.setText(text);
		if (text.equalsIgnoreCase(SimulationExecutionListenerMQTTCameoPluginConstants.CONNECTED)) {
			btnDisconnect.setEnabled(true);
			btnConnect.setEnabled(false);
		} else {
			btnDisconnect.setEnabled(false);
			btnConnect.setEnabled(true);			
		}		
	}
}
