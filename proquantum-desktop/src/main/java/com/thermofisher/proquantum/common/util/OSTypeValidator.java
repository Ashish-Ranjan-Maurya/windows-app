package com.thermofisher.proquantum.common.util;

public class OSTypeValidator {

	private static String osType = System.getProperty("os.name").toLowerCase();
	
	public static boolean isWindows(){
		return (osType.indexOf("win") >= 0);
	}
	
	public static boolean isMac(){
		return (osType.indexOf("mac") >= 0);
	}

	public static boolean isLinux(){
		return (osType.indexOf("linux") >= 0);
	}

}
