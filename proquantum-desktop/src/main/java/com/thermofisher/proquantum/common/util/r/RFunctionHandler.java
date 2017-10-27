package com.thermofisher.proquantum.common.util.r;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;

public class RFunctionHandler {

	public void invokeRprocessCaller(RInput rinput) {

		try {
			String scriptFileExt = FilenameUtils.getExtension(rinput.getScript());
			String msg;

			if ("r".equalsIgnoreCase(scriptFileExt)) {
				DownloadInputFiles dif = new DownloadInputFiles();
				dif.downloadFiles(rinput.getWorkDirLocation(), rinput.getDownloads());

				ExecuteForR efr = new ExecuteForR();
				efr.invoke(dif.getResultPath(), dif.getDownloads(), rinput.getScript(), rinput.getInputs());

				if (efr.getExitVal() != 1) {
					msg = RConstants.SUCCESS;
				} else {
					msg = RConstants.FAILURE;
				}

				File[] outFiles = new File(dif.getResultPath() + RConstants.OUTPUT_DIR).listFiles();

				for (File outFile : outFiles) {
					System.out.println("Provided path : " + outFile.getAbsolutePath());
					// FileUtils.copyFile(outFile, new
					// File(System.getenv("PROQUANTUM_BASE_FOLDER")+"outputFilePath"+outFile.getName()));
					FileUtils.copyFile(outFile,
							new File("C:\\Users\\ashish.ranjan\\Desktop\\proquantum_file_system\\assets\\1234\\"
									+ outFile.getName()));
				}

				//FileUtils.deleteDirectory(new File(dif.getResultPath()));

			}
		} catch (Exception e) {
			// TODO: handle exception
		}
	}

	public static void main(String args[]) throws IOException {

		RInput rinput = new RInput();

		rinput.setWorkDirLocation(Files.createTempDirectory("").toString());
		rinput.setScript(Constants.SCRIPT_NAME);
		
		List<String> downloadList = new ArrayList<String>();
		downloadList.add(0, "C:\\Users\\ashish.ranjan\\Desktop\\proquantum_file_system\\"
				+ Constants.R_SCRIPT_DRIVER_LOCATION);

		String ctResultFilePath = "C:\\Users\\ashish.ranjan\\Desktop\\proquantum_file_system"
				+ Constants.ASSETS + "00073cce-77a0-4570-900a-494450deb262" + "\\ct-results.csv";

		String plateSetUpFilePath = "C:\\Users\\ashish.ranjan\\Desktop\\proquantum_file_system"
				+ Constants.ASSETS + "00073cce-77a0-4570-900a-494450deb262" + "\\plate-setup.csv";

		downloadList.add(1, ctResultFilePath);
		downloadList.add(2, plateSetUpFilePath);

		downloadList.add(3, "C:\\Users\\ashish.ranjan\\Desktop\\proquantum_file_system\\"
				+ Constants.R_SCRIPT_FUNCTIONS_LOCATION);

		downloadList.add(4, "C:\\Users\\ashish.ranjan\\Desktop\\proquantum_file_system\\"
				+ Constants.PRISM_EXPORT_R_LOCATION);

		downloadList.add(5, "C:\\Users\\ashish.ranjan\\Desktop\\proquantum_file_system\\"
				+ Constants.CONC_RESULTS_TML_LOCATION);

		downloadList.add(6,
				"C:\\Users\\ashish.ranjan\\Desktop\\proquantum_file_system\\" + Constants.R_EXECUTOR);

		rinput.setDownloads(downloadList);

		List<String> inputList = new ArrayList<String>();

		// inputList.add("/Library/Frameworks/R.framework/Versions/3.4/Resources");
		inputList.add(Constants.R_SCRIPT_FUNCTIONS);
		inputList.add(Constants.CT_RESULTS_OBJ);
		inputList.add(Constants.PLATE_SETUP_OBJ);
		// adding for prism export
		inputList.add(Constants.PRISM_EXPORT_R);
		inputList.add(Constants.CONC_RESULTS_TML);
		inputList.add("NA");
		inputList.add("5");
		inputList.add("basic");
		inputList.add("\"15,70-130\"");
		// to pass the orientation to R-script for 3rd party
		inputList.add("eds");
		inputList.add("NA");
		inputList.add("RG-384");

		rinput.setInputs(inputList);

		for (String downloadString : rinput.getDownloads()) {
			System.out.println(downloadString);
		}

		RFunctionHandler rFunctionHandler = new RFunctionHandler();
		rFunctionHandler.invokeRprocessCaller(rinput);
	}

}
