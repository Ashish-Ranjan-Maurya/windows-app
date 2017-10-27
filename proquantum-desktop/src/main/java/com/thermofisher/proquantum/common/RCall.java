package com.thermofisher.proquantum.common;

import java.io.BufferedReader;
import java.io.InputStreamReader;

public class RCall {

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		System.out.println("In Main");
		BufferedReader reader=null;
		Process shell=null;
		try{
			/*
			shell=Runtime.getRuntime().exec(new String[]{"C:\\Users\\ashishr\\Documents\\R_Local\\run.bat",
					"C:\\Program Files\\R\\R-3.4.1\\bin\\Rscript.exe","AshishRanjan",
					"Rahul"});
					*/
			String[] paramForR=new String[16];
			paramForR[0] = "C:\\Users\\ashish.ranjan\\Desktop\\Ashish\\desktop-app\\R_WorkSpace\\input\\run.bat";
			//paramForR[1] = "C:\\Program Files\\R\\R-3.4.1\\bin\\Rscript.exe";
			paramForR[1] ="C:\\Users\\ashish.ranjan\\Documents\\R\\R-3.4.2\\bin\\Rscript.exe";
			paramForR[2] = "C:\\Users\\ashish.ranjan\\Desktop\\Ashish\\desktop-app\\R_WorkSpace\\input\\deprendo-5pl-aws-driver.r";
			paramForR[3] = "C:\\Users\\ashish.ranjan\\Desktop\\Ashish\\desktop-app\\R_WorkSpace\\input\\deprendo-5pl-aws.r";
			paramForR[4] = "C:\\Users\\ashish.ranjan\\Desktop\\Ashish\\desktop-app\\R_WorkSpace\\input\\ct-results.csv";
			paramForR[5] = "C:\\Users\\ashish.ranjan\\Desktop\\Ashish\\desktop-app\\R_WorkSpace\\input\\plate-setup.csv";
			paramForR[6] ="C:\\Users\\ashish.ranjan\\Desktop\\Ashish\\desktop-app\\R_WorkSpace\\input\\prism-export.R";
			paramForR[7] ="C:\\Users\\ashish.ranjan\\Desktop\\Ashish\\desktop-app\\R_WorkSpace\\input\\conc-results.tml";
			paramForR[8] ="NA";
			paramForR[9] ="5";
			paramForR[10] ="basic";
			paramForR[11] ="\"15,70-130\"";
			paramForR[12] ="eds";
			paramForR[13] ="NA";
			paramForR[14] ="RG-96";
			paramForR[15] ="C:\\Users\\ashish.ranjan\\Desktop\\Ashish\\desktop-app\\R_WorkSpace\\output";
			
			shell=Runtime.getRuntime().exec(paramForR);
					
			
			
			reader=new BufferedReader(new InputStreamReader(shell.getInputStream()));
			String line;
			while((line=reader.readLine())!=null){
				System.out.println(line);
			}
			//"
		}catch(Exception e){
			
		}

	}

}
