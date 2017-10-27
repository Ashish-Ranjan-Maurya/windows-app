/**
 * 
 */
package com.thermofisher.proquantum.common.util;

import java.util.UUID;

/**
 * @author jeena.joseph
 *
 */
public  class AssetIdUtil {
	
	public static String generateUUID()
	{
		String assetId = UUID.randomUUID().toString();
		return assetId;
	}

}
