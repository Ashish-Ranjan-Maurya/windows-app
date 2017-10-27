package com.thermofisher.proquantum.common.util.r;

import java.io.File;

/**
 * Created by ngadmin on 2/8/16.
 */
@SuppressWarnings("checkstyle:javadocvariable")
public final class Constants {

    /**
     * Private constuctor to avoid creation of new object as
     * all the values in this class are constant.
     */
    private Constants() {

    }
    public static final String CT_RESULTS_OBJ = "ct-results.csv";
    public static final String PLATE_SETUP_OBJ = "plate-setup.csv";
   
    public static final String R_SCRIPT_DRIVER_LOCATION = "R-script-resources"+ File.separator + "deprendo-5pl-aws-driver.r";
    public static final String R_SCRIPT_FUNCTIONS_LOCATION = "R-script-resources" +File.separator + "deprendo-5pl-aws.r";
    
    public static final String SCRIPT_NAME = "deprendo-5pl-aws-driver.r";
    public static final String ASSETS = File.separator +"assets" +File.separator;
    public static final String R_SCRIPT_FUNCTIONS = "deprendo-5pl-aws.r";
    public static final String PRISM_EXPORT_R_LOCATION="R-script-resources" +File.separator +"prism-export.R";
    public static final String CONC_RESULTS_TML_LOCATION="R-script-resources" +File.separator +"conc-results.tml";
    public static final String PRISM_EXPORT_R="prism-export.R";
    public static final String CONC_RESULTS_TML="conc-results.tml";
    public static final String R_EXECUTOR="R-script-resources" +File.separator +"run.bat";
    
    
    /**
     * Enum to define the lambda function names as constants.
     */
    public enum LambdaFunctionName {
        ANALYSIS_LAMBDA("sherpa-analysis-service-v4-4"),
        R_SCRIPT_LAMBDA("analysisFramework-universal"),
        WORKFLOW_LAMBDA("deprendo-workflow-function");

        private final String value;

        /**
         * enum constructor.
         * @param value the name of lambda function
         */
        private LambdaFunctionName(final String value) {
            this.value = value;
        }

        /**
         * Method to get the value of lambda function name from enum.
         * @return the name of lambda function.
         */
        public String getValue() {
            return value;
        }

    }
}
