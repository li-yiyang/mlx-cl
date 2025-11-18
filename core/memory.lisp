;;;; memory.lisp --- Memory Management

(in-package :mlx-cl)

(defun clear-cache ()
  "Clear the memory cache. "
  (ensure-success "mlx_clear_cache"))

(defun get-active-memory ()
  "Get the actively used memory in bytes.
Return active memory size.

Note: this will not always match memory use reported by the system
because it does not include cached memory buffers."
  (with-elem& (size size& :type :size)
    (ensure-success "mlx_get_active_memory" :pointer size&)
    (the integer size)))

(defun get-peak-memory ()
  "Get the peak amount of used memory in bytes.
Return the maximum memory used recorded from the beginning of the program execution
or since the last call to `reset-peak-memory'. "
  (with-elem& (size size& :type :size)
    (ensure-success "mlx_get_peak_memory" :pointer size&)
    (the integer size)))

(defun reset-peak-memory ()
  "Reset the peak memory to zero. "
  (ensure-success "mlx_reset_peak_memory"))

(defun get-cache-memory ()
  "Get the cache size in bytes.
Return the cache memory size.

Note: the cache includes memory not currently used that has not
been returned to the system allocator."
  (with-elem& (size size& :type :size)
    (ensure-success "mlx_get_cache_memory" :pointer size&)
    (the integer size)))

(defun set-memory-limit (limit)
  "Set the memory limit.
Return the previous memory limit in bytes.

Parameters:
+ LIMIT: memory limit in bytes

Note: the memory limit is a guideline for the maximum amount of memory
to use during graph evaluation. If the memory limit is exceeded and there
is no more RAM (including swap when available) allocations will
result in an exception.

When metal is available the memory limit defaults to 1.5 times the maximum
recommended working set size reported by the device. "
  (declare (type (integer 0) limit))
  (with-elem& (size size& :type :size)
    (ensure-success "mlx_set_memory_limit" :pointer size& :size limit)
    (the integer size)))

(defun set-cache-limit (limit)
  "Set the free cache limit in bytes.
Return the previous cache limit in bytes.

Parameters:
+ LIMIT: cache limit in bytes
  + 0: disable cache

Note: if using more than the given limit, free memory will be reclaimed
from the cache on the next allocation.

The cache limit defaults to the memory limit. See `set-memory-limit' for more details."
  (declare (type (integer 0) limit))
  (with-elem& (size size& :type :size)
    (ensure-success "mlx_set_cache_limit" :pointer size& :size limit)
    (the integer size)))

(defun set-wired-limit (limit)
  "Set the wired size limit.
Return the previous wired limit in bytes.

Parameters:
+ LIMIT: wired limit in bytes (default 0)

Note: the wired limit is the total size in bytes of memory that will
be kept resident.

Setting a wired limit larger than system wired limit is an error.
You can increase the system wired limit with:

    sudo sysctl iogpu.wired_limit_mb=<size_in_megabytes>

Use `metal-device-info' to query the system wired limit
`metal-device-max-recommanded-working-set-size') and the total memory
size `metal-device-memory-size'.
"
  (declare (type (integer 0) limit))
  (with-elem& (size size& :type :size)
    (ensure-success "mlx_set_cache_limit" :pointer size& :size limit)
    (the integer size)))

(defun gc-all (&optional (repeat 1))
  "Call both lisp GC and MLX `clear-cache'. "
  (declare (type (integer 1) repeat))
  (dotimes (i repeat)
    (tg:gc :full t)
    (clear-cache)))

;;;; memory.lisp ends here
