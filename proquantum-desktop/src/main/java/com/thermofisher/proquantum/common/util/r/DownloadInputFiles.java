/**
 * 
 */
package com.thermofisher.proquantum.common.util.r;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author ashish.ranjan
 *
 */
public class DownloadInputFiles {

	private String resultPath;
	private Map<String, String> downloads = new HashMap<String, String>();

	public void downloadFiles(String worDirLocation, List<String> inputDownloadList) throws Exception {

		try {

			this.resultPath = worDirLocation;
			System.out.println("temp path : " + this.resultPath);

			downloadFilesFromURL(inputDownloadList);

		} catch (IOException e) {
			System.out.println(e.getMessage());
			System.out.println("Error occured during copy of URL to File : " + e.getStackTrace());
			throw e;
		}
	}

	private void downloadFilesFromURL(List<String> inputDownloadList) throws Exception {

		for (String url : inputDownloadList) {
			try {
				System.out.println("URL=== " +url);
				String fileName = RConstants.generateFileName(url);
				if (!fileName.isEmpty()) {
					File downloadFile = new File(this.resultPath + RConstants.INPUT_DIR + File.separator + fileName);

					File parentFile = downloadFile.getParentFile();
					if (!parentFile.exists() && !parentFile.mkdirs()) {
						System.out.println("Error while creating the parent folders for temp path");
					}

					// For desktop application

					DownloadDependency.copyFileToFolder(new File(url), downloadFile);

					RConstants.providePermission(downloadFile);
					// downloadFile.deleteOnExit();
					System.out.println("input file path : " + downloadFile.getAbsolutePath());

					downloads.put(fileName, downloadFile.getAbsolutePath());
					System.out.println("key : " + fileName + " value : " + downloadFile.getAbsolutePath());
				}
			} catch (IOException e) {
				e.printStackTrace();
				System.out.println("Error occured during copy of URL to File : " + e.getStackTrace());
				throw e;
			} catch (Exception e) {
				System.out.println("Error while downloading the input files");
				e.printStackTrace();
				throw e;
			}
		}
	}

	public String getResultPath() {
		return resultPath;
	}

	public void setResultPath(String resultPath) {
		this.resultPath = resultPath;
	}

	public Map<String, String> getDownloads() {
		return downloads;
	}

	public void setDownloads(Map<String, String> downloads) {
		this.downloads = downloads;
	}
}
