/**
 * 
 */
package com.thermofisher.proquantum.common.util.r;

import java.io.File;

/**
 * @author ashish.ranjan
 *
 */
public class RConstants {
	
	public static final String SUCCESS = "Success";
	
	public static final String FAILURE = "Fail";
	
	public static final String ERROR_FILE = "errorlog.txt";
	
	
	
	
	
	public static final String INPUT_DIR = File.separator +"input";
	
	
	
	public static final String OUTPUT_DIR = File.separator +"output";
	
	
	

	public static String generateFileName(String fullName) {
		if (!fullName.isEmpty()) {
			String[] part = fullName.split(File.separator+ File.separator);
			if (part.length > 1) {
				return part[part.length - 1];
			}else{
				return "";
			}
		}
		return "";
	}
	
	public static void providePermission(File file){
		file.setExecutable(true);
		file.setWritable(true);
		file.setReadable(true);
	}
	
	public static void createOutputDir(String resultPath) {

		File outFile = new File(resultPath + RConstants.OUTPUT_DIR);
		outFile.mkdir();
		outFile.setWritable(true);
	}
}
