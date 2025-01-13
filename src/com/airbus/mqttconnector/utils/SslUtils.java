/*
* Copyright (c) 2025 AIRBUS and its affiliates.
* This Source Code Form is subject to the terms of the Mozilla Public
* License, v. 2.0. If a copy of the MPL was not distributed with this
* file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

package com.airbus.mqttconnector.utils;

import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.security.KeyStore;
import java.security.Security;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;

import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSocketFactory;
import javax.net.ssl.TrustManagerFactory;

import org.bouncycastle.jce.provider.BouncyCastleProvider;

public class SslUtils {

	private SslUtils() {
		
	}
	
    public static SSLSocketFactory getSingleSocketFactory(final String caCrtFile) throws Exception {
        Security.addProvider(new BouncyCastleProvider());
        X509Certificate caCert = null;

        FileInputStream caCrtFileInputStream = new FileInputStream(caCrtFile);

        try (BufferedInputStream bis = new BufferedInputStream(caCrtFileInputStream)) {
	        CertificateFactory cf = CertificateFactory.getInstance("X.509");
	
	        while (bis.available() > 0) {
	            caCert = (X509Certificate) cf.generateCertificate(bis);
	        }
        }
        
        KeyStore caKs = KeyStore.getInstance(KeyStore.getDefaultType());
        caKs.load(null, null);
        caKs.setCertificateEntry("cert-certificate", caCert);
        TrustManagerFactory tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm());
        tmf.init(caKs);
        SSLContext sslContext = SSLContext.getInstance("TLSv1.2");
        sslContext.init(null, tmf.getTrustManagers(), null);

        return sslContext.getSocketFactory();
    }
}
