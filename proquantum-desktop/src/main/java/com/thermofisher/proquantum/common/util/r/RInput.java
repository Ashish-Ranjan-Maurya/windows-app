package com.thermofisher.proquantum.common.util.r;

import java.util.List;

public class RInput {

	private List<String > downloads;
	private String dependency;
	private String script;
	private List<String> inputs;
	private String outputBucket;
	private String workDirLocation;
	/**
	 * @return the downloads
	 */
	public List<String> getDownloads() {
		return downloads;
	}
	/**
	 * @param downloads the downloads to set
	 */
	public void setDownloads(List<String> downloads) {
		this.downloads = downloads;
	}
	/**
	 * @return the dependency
	 */
	public String getDependency() {
		return dependency;
	}
	/**
	 * @param dependency the dependency to set
	 */
	public void setDependency(String dependency) {
		this.dependency = dependency;
	}
	/**
	 * @return the script
	 */
	public String getScript() {
		return script;
	}
	/**
	 * @param script the script to set
	 */
	public void setScript(String script) {
		this.script = script;
	}
	/**
	 * @return the inputs
	 */
	public List<String> getInputs() {
		return inputs;
	}
	/**
	 * @param inputs the inputs to set
	 */
	public void setInputs(List<String> inputs) {
		this.inputs = inputs;
	}
	/**
	 * @return the outputBucket
	 */
	public String getOutputBucket() {
		return outputBucket;
	}
	/**
	 * @param outputBucket the outputBucket to set
	 */
	public void setOutputBucket(String outputBucket) {
		this.outputBucket = outputBucket;
	}
	/**
	 * @return the workDirLocation
	 */
	public String getWorkDirLocation() {
		return workDirLocation;
	}
	/**
	 * @param workDirLocation the workDirLocation to set
	 */
	public void setWorkDirLocation(String workDirLocation) {
		this.workDirLocation = workDirLocation;
	}
	
	
}
