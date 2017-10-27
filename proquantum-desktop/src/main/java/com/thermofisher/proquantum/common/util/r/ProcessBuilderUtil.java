package com.thermofisher.proquantum.common.util.r;

import java.io.File;
import java.io.IOException;

/**
 * Created by Sandeep Choudhari on 19/10/16.
 */
public class ProcessBuilderUtil {

    public static Process getProcessObject(String [] params,String resultPath) throws IOException{
        Process process = null;
        File errorFile = new File(resultPath + RConstants.OUTPUT_DIR + File.separator + RConstants.ERROR_FILE);
        ProcessBuilder pb = new ProcessBuilder(params);

        pb.directory(new File(resultPath+ RConstants.INPUT_DIR + File.separator));
        pb.redirectErrorStream(true);
        pb.redirectOutput(errorFile);
        try {
             process = pb.start();
        }catch (IOException e){
            System.out.println("Exception while starting the process to run R script"+e.getMessage());
            e.printStackTrace();
            throw e;
        }
        return process;
    }
}
