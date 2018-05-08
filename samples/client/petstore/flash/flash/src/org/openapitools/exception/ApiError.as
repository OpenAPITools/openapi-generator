package org.openapitools.exception
{
	public class ApiError extends Error
	{		
		public function ApiError(id:*=0, message:*="")
		{
			super(message,id);
		}
	}
}