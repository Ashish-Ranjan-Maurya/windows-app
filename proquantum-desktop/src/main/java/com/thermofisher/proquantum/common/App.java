package com.thermofisher.proquantum.common;

import java.io.File;

import com.thermofisher.proquantum.common.util.r.RConstants;

/**
 * Hello world!
 *
 */
public class App 
{
    public static void main( String[] args )
    {
        System.out.println( "Hello World!" );
        String str="C:\\Users\\ashish.ranjan\\Desktop\\proquantum_file_system\\R-script-resources\\deprendo-5pl-aws-driver.r";
       String fileName=generateFileName(str);
       
       System.out.println(fileName);
       
    }
    
    public static String generateFileName(String fullName) {
    	System.out.println("fullName==" +fullName);
		if (!fullName.isEmpty()) {
			String[] part = fullName.split(File.separator +File.separator);
			if (part.length > 1) {
				return part[part.length - 1];
			}else{
				return "";
			}
		}
		return "";
	}
}
