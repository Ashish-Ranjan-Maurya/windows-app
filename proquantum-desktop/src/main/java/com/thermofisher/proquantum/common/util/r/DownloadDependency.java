/**
 * 
 */
package com.thermofisher.proquantum.common.util.r;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;

/**
 * @author ashish.ranjan
 *
 */
public class DownloadDependency {
	
	/**
	 * Size of the buffer to read/write data
	 */
	private static final int BUFFER_SIZE = 4096;

  
	

	
	
    private static String getBucketName(String dependencyRelativeFilePath){

        String bucketName = dependencyRelativeFilePath.substring(0,dependencyRelativeFilePath.indexOf("/"));
        System.out.println("BucketName is :"+bucketName);
        return bucketName;

    }

   

    /**
	 * Extracts a zip file specified by the zipFilePath to a directory specified by
	 * destDirectory (will be created if does not exists)
	 * @param zipFilePath
	 * @param destDirectory
	 * @throws IOException
	 */
	private void unZip(String zipFilePath, String destDirectory) throws IOException {
		File destDir = new File(destDirectory);
		if (!destDir.exists()) {
			destDir.mkdir();
			RConstants.providePermission(destDir);
		}
		ZipInputStream zipIn = new ZipInputStream(new FileInputStream(zipFilePath));
		ZipEntry entry = zipIn.getNextEntry();
		// iterates over entries in the zip file
		while (entry != null) {
			String filePath = destDirectory + File.separator + entry.getName();
			//           System.out.println("Extracted file : "+ filePath);
			if (!entry.isDirectory()) {
				// if the entry is a file, extracts it
				extractFile(zipIn, filePath);
			} else {
				// if the entry is a directory, make the directory
				File dir = new File(filePath);
				dir.mkdir();
				RConstants.providePermission(dir);
				dir.deleteOnExit();
			}
			zipIn.closeEntry();
			entry = zipIn.getNextEntry();
		}
		zipIn.close();
	}
	/**
	 * Extracts a zip entry (file entry)
	 * @param zipIn
	 * @param filePath
	 * @throws IOException
	 */
	private void extractFile(ZipInputStream zipIn, String filePath) throws IOException {
		File f= new File(filePath);
		RConstants.providePermission(f);
		f.deleteOnExit();
		FileOutputStream fo = new FileOutputStream(f);
		BufferedOutputStream bos = new BufferedOutputStream(fo);
		byte[] bytesIn = new byte[BUFFER_SIZE];
		int read = 0;
		while ((read = zipIn.read(bytesIn)) != -1) {
			bos.write(bytesIn, 0, read);
		}
		bos.close();
	}
	
	
	public static void copyFileToFolder(File sourceFile,File destFile) throws IOException{
		FileUtils.copyFile(sourceFile,destFile);
	}
}
