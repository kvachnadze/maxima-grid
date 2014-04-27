(defun write-list-lowlevel (L out sep-ch mode)
  (setq sep-ch (cond ((symbolp sep-ch) (cadr (exploden sep-ch))) (t sep-ch)))
  (cond ((not (null L))
      (loop 
        (if (not L) (return))
        (let ((e (pop L)))
          (cond (($listp e)
              (write-list-lowlevel (cdr e) out sep-ch mode))
            (t
              (cond
                ((eq mode 'text)
                 (mgrind e out)
		 (write-char sep-ch out))
                ((eq mode 'binary)
                 (if ($numberp e)
                   (write-float ($float e) out)
                   (merror "write_data: encountered non-numeric data in binary output")))
                (t
                  (merror "write_data: unrecognized mode"))))))))))
