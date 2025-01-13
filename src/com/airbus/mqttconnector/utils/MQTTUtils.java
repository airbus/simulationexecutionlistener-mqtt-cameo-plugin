/*
* Copyright (c) 2025 AIRBUS and its affiliates.
* This Source Code Form is subject to the terms of the Mozilla Public
* License, v. 2.0. If a copy of the MPL was not distributed with this
* file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

package com.airbus.mqttconnector.utils;

import java.security.SecureRandom;
import java.util.Random;
import java.util.logging.Logger;

public class MQTTUtils {

	public static final Logger log = Logger.getLogger(MQTTUtils.class.getName());
	private static Random random = new SecureRandom();
	
	private MQTTUtils() {
		
	}
	
	public static String generateRandomNumberString(int charLength) {
        return String.valueOf(charLength < 1 ? 0 : random.nextInt((9 * (int) Math.pow(10, charLength - 1.0)) - 1) + (int) Math.pow(10, charLength - 1.0));
    }
	
}
