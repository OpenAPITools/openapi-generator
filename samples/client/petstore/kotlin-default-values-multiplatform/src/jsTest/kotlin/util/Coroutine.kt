package util

import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.GlobalScope
import kotlinx.coroutines.promise

actual fun <T> runTest(block: suspend (scope : CoroutineScope) -> T): dynamic = GlobalScope.promise { block(this) }
