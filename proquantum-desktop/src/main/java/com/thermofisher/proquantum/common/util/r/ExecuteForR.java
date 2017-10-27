/**
 * 
 */
package com.thermofisher.proquantum.common.util.r;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.thermofisher.proquantum.common.util.OSTypeValidator;


/**
 * @author rohit.chaturvedi
 *
 */
public class ExecuteForR {

	private int exitVal;	
	private static final Logger LOGGER = LoggerFactory.getLogger(ExecuteForR.class);
	/**
	 * @param resultPath
	 * @param downloadsMap
	 * @param script
	 * @param list
     * @param omittedSamples
	 * @throws IOException
	 */
	public void invoke(String resultPath, Map<String, String> downloadsMap, String script, List<String> inputList) throws Exception {
		// TODO Auto-generated method stub
		
		System.out.println("===resultPath=== " +resultPath);
		for (Map.Entry<String, String> entry : downloadsMap.entrySet()) {
		    System.out.println("Key = " + entry.getKey() + ", Value = " + entry.getValue());
		}
		System.out.println("===script=== "+script);
		for(String input:inputList){
			System.out.println("==input=="+input);
		}
		
		
		RConstants.createOutputDir(resultPath);

		//method for Deprendo
		invokeScriptFromR(resultPath, downloadsMap, script, inputList);
	}
	
	private void invokeScriptFromR(String resultPath, Map<String, String> downloadsMap, String script, List<String> inputs) throws Exception{

		//String rExecutorFileName=getRExecutorFileName();
		//String R_HOME_LOCATION=System.getenv("R_HOME");
		
		String rExecutorFileName=getRExecutorFileName();
		//String R_HOME_LOCATION="/usr/lib/R/bin";
				
		String R_HOME_LOCATION="C:\\Users\\ashish.ranjan\\Documents\\R\\R-3.4.2\\bin\\Rscript.exe";
		File shFile = new File(resultPath+ RConstants.INPUT_DIR + File.separator + rExecutorFileName);
		
		shFile.setExecutable(true);
		
		Process process = null;
		try{
			//sh file + R script + inputs + outDest
			String [] params = new String [1 + 1 +inputs.size() + 1 + 1];

			for(int i=0; i< params.length; i++){
				if(i==0)
					params[i] = shFile.getAbsolutePath();
				else if(i==1){
					//String osType=System.
					//params[i] =  "/Library/Frameworks/R.framework/Versions/3.4/Resources";
					params[i]=R_HOME_LOCATION;
				}else if(i==2 && downloadsMap.containsKey(script)){
					params[i] =  downloadsMap.get(script);
					downloadsMap.remove(script);
				}else if(i == (params.length -1)) {
                    params[i] = resultPath + RConstants.OUTPUT_DIR;
                }else{
					for(String in : inputs){
						if(downloadsMap.containsKey(in)){
							params[i++] = downloadsMap.get(in);
							downloadsMap.remove(in);
						}else{
							params[i++] = in;
						}
					}
					i--;
				}
				
			}

			for(String paramsForR:params){
				LOGGER.info("The value in the params is " + paramsForR);
			}

            process = ProcessBuilderUtil.getProcessObject(params,resultPath);
           
            
            int shellExitStatus = process.waitFor();
            
            System.out.println("Exit status: " + shellExitStatus);
            //The below lines are just for testing purpose to see the various output parameters
            //This is IO operation and it does not make sense to keep the code.
            //Currently commenting out the code.

			/*
			InputStream stderr = process.getInputStream();
			InputStreamReader isr = new InputStreamReader(stderr);
			BufferedReader br = new BufferedReader(isr);
			String line = null;

			while ((line = br.readLine()) != null) {
				System.out.println(line);
			}
			br.close();
			isr.close();
			stderr.close();*/

		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			System.out.println(e.getMessage());
            throw e;
		}  catch (IOException e){
			System.out.println(e.getMessage());
            throw e;
		} catch (Exception e){
			System.out.println(e.getMessage());
            throw e;
		}

		System.out.println("Waiting ...");
		this.exitVal = process.exitValue();
		System.out.println("Returned Value :" + getExitVal());
	}
	
	public int getExitVal() {
		return exitVal;
	}
	
	public String getRExecutorFileName(){

		if(OSTypeValidator.isLinux()){
			return "run_mac.sh";
		}
		if(OSTypeValidator.isMac()){
			return "run_mac.sh";
		}
		if(OSTypeValidator.isWindows()){
			return "run.bat";
		}
		
		return "";
	}
	
	
	
	public static void main(String args[]){
		//String osType=System.getProperty("os.name").toLowerCase();
		
		String osType="Linux".toLowerCase();
		
		boolean osflag=(osType.indexOf("mac")>=0);
		
		boolean osflag1=(osType.indexOf("linux")>=0);
		
		System.out.println(osflag +"====="+osflag1);
		
	}
	
	
}
