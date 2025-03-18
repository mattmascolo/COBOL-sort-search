       identification division.
       program-id.  sort-search.
       author.  sec9.

       environment division.
       input-output section.
       file-control.
         select array-file assign to "MOI-Proj.txt"
            organization is line sequential.

       data division.
       file section.
       fd array-file.
       01 array-file-record.
           05  file-idx     pic     99.
           05  file-pname   pic     x(38).

       working-storage section.

       01  eof         pic     x       value 'n'.
       01  user-choice pic     x       value '0'.

       01  project-array  occurs 100 times indexed by arr-index.
           05  idx     pic 99.
           05  pname   pic x(38).

       01  num             pic     999     value 0.
       01  i               pic     999     value 0.
       01  idx-out         pic     99.
       01  idx-in          pic     999     value 0.

       01  imax             pic     999     value 0.
       01  imin             pic     999     value 0.
       01  imid             pic     999     value 0.

       01  temp.
           05  temp-idx    pic 99.
           05  temp-pname  pic x(38).
       01  swaps           pic 9   value 1.

       procedure division.
       000-main.
           perform 100-load-file
           perform 200-display-menu
           perform until user-choice = 'E'
               evaluate user-choice
                   when '1'
                       perform 300-display-array
                   when '2'
                       perform 400-bubble-sort
                   when '3'
                       perform 500-binary-search
               end-evaluate
               perform 200-display-menu
           end-perform
           display "Goodbye."
           stop run.

       100-load-file.
           open input array-file
           perform until eof = 'y'
               read array-file
                 at end
                   move 'y' to eof
                 not at end
                   add 1 to num
                   move file-idx to idx(num)
                   move file-pname to pname(num)
               end-read
           end-perform
           close array-file.

       200-display-menu.
           display "----"
           display "Menu"
           display "----"
           display "1 - Show Table"
           display "2 - Sort"
           display "3 - Search"
           display " "
           display "E - Exit"
           display " "
           accept user-choice.

       300-display-array.
           display "Project list"
           display "------------"
           perform varying i from 1 by 1 until i > num
               move idx(i) to idx-out
               display idx-out, "   ", pname(i)
           end-perform.

       400-bubble-sort.
           perform until swaps = 0
               move 0 to swaps
               perform varying i from 1 by 1 until i < num
                   if idx(i) > idx(i + 1)
                       move idx(i) to temp-idx
                       move pname(i) to temp-pname
                       move idx(i + 1) to idx(i)
                       move pname(i + 1) to pname(i)
                       move temp-idx to idx(i + 1)
                       move temp-pname to pname(i + 1)
                       move 1 to swaps
                   end-if
               end-perform
           end-perform.

       500-binary-search.
           display "Enter id to search for: " with no advancing
           accept idx-in

           move num to imax
           move 1 to imin

           perform until imin > imax
               compute imid = (imax + imin) / 2
               if imid < 1 then move 1 to imid
               
               if idx(imid) > idx-in
                   compute imax = imid - 1
               else
                   if idx(imid) < idx-in
                       compute imin = imid + 1
                   else
                       display "Found on row: ", imid
                       move num to imin
                       move 1 to imax
                   end-if
               end-if
           end-perform.
