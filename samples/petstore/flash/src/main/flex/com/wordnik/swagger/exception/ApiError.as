package com.wordnik.swagger.exception
{
	public class ApiError extends Error
	{		
		public function ApiError(id:*=0, message:*="")
		{
			super(message,id);
		}
	}
}