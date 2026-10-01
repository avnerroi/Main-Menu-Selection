
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SELECTION-FUNCTIONS.

      *> Programmed by: Roi


      *> The four function categories are:
      *> 1. No argument, no return value
      *> 2. Arguments passed, no return value
      *> 3. No arguments, returns a value
      *> 4. Arguments passed, returns a value
      *>
      *> COBOL has no real arguments or return values, so:
      *>   - "arguments" are working-storage variables that are filled
      *>     in BEFORE the paragraph is PERFORMed (GET-... paragraphs).
      *>   - "return values" are left in RET-NUM / RET-INT / RET-CHAR.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  MAIN-CHOICE             PIC 99 VALUE 0.
       01  SUB-CHOICE              PIC 99 VALUE 0.

      *> ---------------- "ARGUMENT" VARIABLES --------------------------
       01  PRELIM                  PIC S9(5)V99 VALUE 0.
       01  MIDTERM                 PIC S9(5)V99 VALUE 0.
       01  FINAL-GRADE             PIC S9(5)V99 VALUE 0.
       01  AVG-VALUE               PIC S9(5)V99 VALUE 0.
       01  NUM-VALUE               PIC S9(9)V99 VALUE 0.
       01  INT-A                   PIC S9(9) VALUE 0.
       01  INT-B                   PIC S9(9) VALUE 0.
       01  INT-C                   PIC S9(9) VALUE 0.
       01  TEMP-VALUE              PIC S9(5)V99 VALUE 0.
       01  SALESMAN-NUMBER         PIC 9(9) VALUE 0.
       01  SALESMAN-NAME           PIC X(20) VALUE SPACES.
       01  UNIT-SOLD               PIC S9(7)V99 VALUE 0.
       01  UNIT-PRICE              PIC S9(7)V99 VALUE 0.
       01  COMMISSION              PIC S9(9)V99 VALUE 0.
       01  DAY-NUMBER              PIC S9(4) VALUE 0.
       01  LETTER                  PIC X VALUE SPACE.

      *> ---------------- "RETURN VALUE" VARIABLES ----------------------
       01  RET-NUM                 PIC S9(9)V99 VALUE 0.
       01  RET-INT                 PIC S9(9) VALUE 0.
       01  RET-CHAR                PIC X VALUE SPACE.

      *> ---------------- EDITED FIELDS FOR NEAT OUTPUT -----------------
       01  ED-DEC                  PIC -(9)9.99.
       01  ED-INT                  PIC -(9)9.
       01  ED-GRADE                PIC 9.99.

       PROCEDURE DIVISION.

      *> ================================================================
      *> MAIN MENU
      *> ================================================================
       MAIN-PROGRAM.
           PERFORM UNTIL MAIN-CHOICE = 5
               DISPLAY " "
               DISPLAY "MAIN MENU"
               DISPLAY "1. No Argument + No Return"
               DISPLAY "2. Argument + No Return"
               DISPLAY "3. No Argument + Return"
               DISPLAY "4. Argument + Return"
               DISPLAY "5. Exit"
               DISPLAY "Enter your choice: "
               ACCEPT MAIN-CHOICE

               EVALUATE MAIN-CHOICE
                   WHEN 1
                       PERFORM FUNCTION-1-MENU
                   WHEN 2
                       PERFORM FUNCTION-2-MENU
                   WHEN 3
                       PERFORM FUNCTION-3-MENU
                   WHEN 4
                       PERFORM FUNCTION-4-MENU
                   WHEN 5
                       DISPLAY "Goodbye!"
                   WHEN OTHER
                       DISPLAY "Invalid choice. Try again."
               END-EVALUATE
           END-PERFORM

           STOP RUN.

      *> Shared list of the ten items shown in every function menu.
       SHOW-FUNCTION-LIST.
           DISPLAY "1. Revised Grade"
           DISPLAY "2. Positive or Negative"
           DISPLAY "3. Odd or Even"
           DISPLAY "4. Largest"
           DISPLAY "5. Smallest"
           DISPLAY "6. Equivalent Grade"
           DISPLAY "7. Temperature"
           DISPLAY "8. Sales"
           DISPLAY "9. Days"
           DISPLAY "10. Alphabet"
           DISPLAY "11. Back"
           DISPLAY "Enter your choice: ".

      *> ================================================================
      *> FUNCTION 1 MENU - NO ARGUMENT AND NO RETURN VALUE
      *> Each paragraph asks for its own input and prints its result.
      *> ================================================================
       FUNCTION-1-MENU.
           MOVE 0 TO SUB-CHOICE

           PERFORM UNTIL SUB-CHOICE = 11
               DISPLAY " "
               DISPLAY "FUNCTION 1 MENU"
               PERFORM SHOW-FUNCTION-LIST
               ACCEPT SUB-CHOICE

               EVALUATE SUB-CHOICE
                   WHEN 1
                       PERFORM REVISED-GRADE-1
                   WHEN 2
                       PERFORM POS-NEG-1
                   WHEN 3
                       PERFORM ODD-EVEN-1
                   WHEN 4
                       PERFORM LARGEST-1
                   WHEN 5
                       PERFORM SMALLEST-1
                   WHEN 6
                       PERFORM EQUIV-GRADE-1
                   WHEN 7
                       PERFORM TEMPERATURE-1
                   WHEN 8
                       PERFORM SALES-1
                   WHEN 9
                       PERFORM DAYS-1
                   WHEN 10
                       PERFORM ALPHABET-1
                   WHEN 11
                       CONTINUE
                   WHEN OTHER
                       DISPLAY "Invalid choice. Try again."
               END-EVALUATE
           END-PERFORM.

       REVISED-GRADE-1.
           PERFORM GET-GRADES
           PERFORM CALC-AVERAGE
           PERFORM SHOW-AVERAGE.

       POS-NEG-1.
           PERFORM GET-NUMBER
           PERFORM CALC-POS-NEG
           PERFORM SHOW-POS-NEG.

       ODD-EVEN-1.
           PERFORM GET-INTEGER
           PERFORM CALC-ODD-EVEN
           PERFORM SHOW-ODD-EVEN.

       LARGEST-1.
           PERFORM GET-TWO-INTS
           PERFORM CALC-LARGEST
           PERFORM SHOW-LARGEST.

       SMALLEST-1.
           PERFORM GET-THREE-INTS
           PERFORM CALC-SMALLEST
           PERFORM SHOW-SMALLEST.

       EQUIV-GRADE-1.
           PERFORM GET-AVERAGE
           PERFORM CALC-EQUIV
           PERFORM SHOW-EQUIV.

       TEMPERATURE-1.
           PERFORM GET-TEMPERATURE
           PERFORM CALC-TEMP
           PERFORM SHOW-TEMP.

       SALES-1.
           PERFORM GET-SALES-FULL
           PERFORM CALC-SALES
           PERFORM SHOW-SALES.

       DAYS-1.
           PERFORM GET-DAY
           PERFORM CALC-DAY
           PERFORM SHOW-DAY.

       ALPHABET-1.
           PERFORM GET-LETTER
           PERFORM CALC-LETTER
           PERFORM SHOW-LETTER.

      *> ================================================================
      *> FUNCTION 2 MENU - ARGUMENTS PASSED AND NO RETURN VALUE
      *> The menu (the "caller") fills in the variables first, then
      *> PERFORMs the paragraph, which only prints the result.
      *> ================================================================
       FUNCTION-2-MENU.
           MOVE 0 TO SUB-CHOICE

           PERFORM UNTIL SUB-CHOICE = 11
               DISPLAY " "
               DISPLAY "FUNCTION 2 MENU"
               PERFORM SHOW-FUNCTION-LIST
               ACCEPT SUB-CHOICE

               EVALUATE SUB-CHOICE
                   WHEN 1
                       PERFORM GET-GRADES
                       PERFORM REVISED-GRADE-2
                   WHEN 2
                       PERFORM GET-NUMBER
                       PERFORM POS-NEG-2
                   WHEN 3
                       PERFORM GET-INTEGER
                       PERFORM ODD-EVEN-2
                   WHEN 4
                       PERFORM GET-TWO-INTS
                       PERFORM LARGEST-2
                   WHEN 5
                       PERFORM GET-THREE-INTS
                       PERFORM SMALLEST-2
                   WHEN 6
                       PERFORM GET-AVERAGE
                       PERFORM EQUIV-GRADE-2
                   WHEN 7
                       PERFORM GET-TEMPERATURE
                       PERFORM TEMPERATURE-2
                   WHEN 8
                       PERFORM GET-SALES-ARGS
                       PERFORM SALES-2
                   WHEN 9
                       PERFORM GET-DAY
                       PERFORM DAYS-2
                   WHEN 10
                       PERFORM GET-LETTER
                       PERFORM ALPHABET-2
                   WHEN 11
                       CONTINUE
                   WHEN OTHER
                       DISPLAY "Invalid choice. Try again."
               END-EVALUATE
           END-PERFORM.

       REVISED-GRADE-2.
           PERFORM CALC-AVERAGE
           PERFORM SHOW-AVERAGE.

       POS-NEG-2.
           PERFORM CALC-POS-NEG
           PERFORM SHOW-POS-NEG.

       ODD-EVEN-2.
           PERFORM CALC-ODD-EVEN
           PERFORM SHOW-ODD-EVEN.

       LARGEST-2.
           PERFORM CALC-LARGEST
           PERFORM SHOW-LARGEST.

       SMALLEST-2.
           PERFORM CALC-SMALLEST
           PERFORM SHOW-SMALLEST.

       EQUIV-GRADE-2.
           PERFORM CALC-EQUIV
           PERFORM SHOW-EQUIV.

       TEMPERATURE-2.
           PERFORM CALC-TEMP
           PERFORM SHOW-TEMP.

       SALES-2.
           PERFORM CALC-SALES
           PERFORM SHOW-SALES.

       DAYS-2.
           PERFORM CALC-DAY
           PERFORM SHOW-DAY.

       ALPHABET-2.
           PERFORM CALC-LETTER
           PERFORM SHOW-LETTER.

      *> ================================================================
      *> FUNCTION 3 MENU - NO ARGUMENTS AND RETURNS A VALUE
      *> Each paragraph asks for its own input, prints its result and
      *> leaves a value in RET-NUM / RET-INT / RET-CHAR, which the menu
      *> (the "caller") then displays.
      *> ================================================================
       FUNCTION-3-MENU.
           MOVE 0 TO SUB-CHOICE

           PERFORM UNTIL SUB-CHOICE = 11
               DISPLAY " "
               DISPLAY "FUNCTION 3 MENU"
               PERFORM SHOW-FUNCTION-LIST
               ACCEPT SUB-CHOICE

               EVALUATE SUB-CHOICE
                   WHEN 1
                       PERFORM REVISED-GRADE-3
                       PERFORM SHOW-RETURN-NUM
                   WHEN 2
                       PERFORM POS-NEG-3
                       PERFORM SHOW-RETURN-INT
                   WHEN 3
                       PERFORM ODD-EVEN-3
                       PERFORM SHOW-RETURN-INT
                   WHEN 4
                       PERFORM LARGEST-3
                       PERFORM SHOW-RETURN-INT
                   WHEN 5
                       PERFORM SMALLEST-3
                       PERFORM SHOW-RETURN-INT
                   WHEN 6
                       PERFORM EQUIV-GRADE-3
                       PERFORM SHOW-RETURN-NUM
                   WHEN 7
                       PERFORM TEMPERATURE-3
                       PERFORM SHOW-RETURN-INT
                   WHEN 8
                       PERFORM SALES-3
                       PERFORM SHOW-RETURN-NUM
                   WHEN 9
                       PERFORM DAYS-3
                       PERFORM SHOW-RETURN-INT
                   WHEN 10
                       PERFORM ALPHABET-3
                       PERFORM SHOW-RETURN-CHAR
                   WHEN 11
                       CONTINUE
                   WHEN OTHER
                       DISPLAY "Invalid choice. Try again."
               END-EVALUATE
           END-PERFORM.

       REVISED-GRADE-3.
           PERFORM GET-GRADES
           PERFORM CALC-AVERAGE
           PERFORM SHOW-AVERAGE.

       POS-NEG-3.
           PERFORM GET-NUMBER
           PERFORM CALC-POS-NEG
           PERFORM SHOW-POS-NEG.

       ODD-EVEN-3.
           PERFORM GET-INTEGER
           PERFORM CALC-ODD-EVEN
           PERFORM SHOW-ODD-EVEN.

       LARGEST-3.
           PERFORM GET-TWO-INTS
           PERFORM CALC-LARGEST
           PERFORM SHOW-LARGEST.

       SMALLEST-3.
           PERFORM GET-THREE-INTS
           PERFORM CALC-SMALLEST
           PERFORM SHOW-SMALLEST.

       EQUIV-GRADE-3.
           PERFORM GET-AVERAGE
           PERFORM CALC-EQUIV
           PERFORM SHOW-EQUIV.

       TEMPERATURE-3.
           PERFORM GET-TEMPERATURE
           PERFORM CALC-TEMP
           PERFORM SHOW-TEMP.

       SALES-3.
           PERFORM GET-SALES-FULL
           PERFORM CALC-SALES
           PERFORM SHOW-SALES.

       DAYS-3.
           PERFORM GET-DAY
           PERFORM CALC-DAY
           PERFORM SHOW-DAY.

       ALPHABET-3.
           PERFORM GET-LETTER
           PERFORM CALC-LETTER
           PERFORM SHOW-LETTER.

      *> ================================================================
      *> FUNCTION 4 MENU - ARGUMENTS PASSED AND RETURNS A VALUE
      *> The menu fills in the variables, PERFORMs the paragraph (which
      *> only calculates and "returns" a value), then the menu uses the
      *> returned value to print the result.
      *> ================================================================
       FUNCTION-4-MENU.
           MOVE 0 TO SUB-CHOICE

           PERFORM UNTIL SUB-CHOICE = 11
               DISPLAY " "
               DISPLAY "FUNCTION 4 MENU"
               PERFORM SHOW-FUNCTION-LIST
               ACCEPT SUB-CHOICE

               EVALUATE SUB-CHOICE
                   WHEN 1
                       PERFORM GET-GRADES
                       PERFORM REVISED-GRADE-4
                       PERFORM SHOW-AVERAGE
                   WHEN 2
                       PERFORM GET-NUMBER
                       PERFORM POS-NEG-4
                       PERFORM SHOW-POS-NEG
                   WHEN 3
                       PERFORM GET-INTEGER
                       PERFORM ODD-EVEN-4
                       PERFORM SHOW-ODD-EVEN
                   WHEN 4
                       PERFORM GET-TWO-INTS
                       PERFORM LARGEST-4
                       PERFORM SHOW-LARGEST
                   WHEN 5
                       PERFORM GET-THREE-INTS
                       PERFORM SMALLEST-4
                       PERFORM SHOW-SMALLEST
                   WHEN 6
                       PERFORM GET-AVERAGE
                       PERFORM EQUIV-GRADE-4
                       PERFORM SHOW-EQUIV
                   WHEN 7
                       PERFORM GET-TEMPERATURE
                       PERFORM TEMPERATURE-4
                       PERFORM SHOW-TEMP
                   WHEN 8
                       PERFORM GET-SALES-ARGS
                       PERFORM SALES-4
                       PERFORM SHOW-SALES
                   WHEN 9
                       PERFORM GET-DAY
                       PERFORM DAYS-4
                       PERFORM SHOW-DAY
                   WHEN 10
                       PERFORM GET-LETTER
                       PERFORM ALPHABET-4
                       PERFORM SHOW-LETTER
                   WHEN 11
                       CONTINUE
                   WHEN OTHER
                       DISPLAY "Invalid choice. Try again."
               END-EVALUATE
           END-PERFORM.

       REVISED-GRADE-4.
      *> Returns the average in RET-NUM.
           PERFORM CALC-AVERAGE.

       POS-NEG-4.
      *> Returns 1 (positive), -1 (negative) or 0 (neutral).
           PERFORM CALC-POS-NEG.

       ODD-EVEN-4.
      *> Returns 0 (zero), 1 (odd) or 2 (even).
           PERFORM CALC-ODD-EVEN.

       LARGEST-4.
      *> Returns the larger of the two numbers.
           PERFORM CALC-LARGEST.

       SMALLEST-4.
      *> Returns the smallest of the three numbers.
           PERFORM CALC-SMALLEST.

       EQUIV-GRADE-4.
      *> Returns the equivalent grade (0 means invalid average).
           PERFORM CALC-EQUIV.

       TEMPERATURE-4.
      *> Returns a weather code from 1 (freezing) to 6 (very hot).
           PERFORM CALC-TEMP.

       SALES-4.
      *> Returns the total sales.
           PERFORM CALC-SALES.

       DAYS-4.
      *> Returns the day number, or 0 when the day is invalid.
           PERFORM CALC-DAY.

       ALPHABET-4.
      *> Returns V (vowel), C (consonant) or N (not a letter).
           PERFORM CALC-LETTER.

      *> ================================================================
      *> GET-... PARAGRAPHS (these supply the "arguments")
      *> ================================================================
       GET-GRADES.
           DISPLAY "Input prelim: "
           ACCEPT PRELIM
           DISPLAY "Input midterm: "
           ACCEPT MIDTERM
           DISPLAY "Input final: "
           ACCEPT FINAL-GRADE.

       GET-NUMBER.
           DISPLAY "Enter a number: "
           ACCEPT NUM-VALUE.

       GET-INTEGER.
           DISPLAY "Enter a number: "
           ACCEPT INT-A.

       GET-TWO-INTS.
           DISPLAY "Enter a number 1: "
           ACCEPT INT-A
           DISPLAY "Enter a number 2: "
           ACCEPT INT-B.

       GET-THREE-INTS.
           DISPLAY "Enter the value of num1: "
           ACCEPT INT-A
           DISPLAY "Enter the value of num2: "
           ACCEPT INT-B
           DISPLAY "Enter the value of num3: "
           ACCEPT INT-C.

       GET-AVERAGE.
           DISPLAY "Enter your average: "
           ACCEPT AVG-VALUE.

       GET-TEMPERATURE.
           DISPLAY "Input temperature: "
           ACCEPT TEMP-VALUE.

       GET-SALES-FULL.
           DISPLAY "Input salesman_Number: "
           ACCEPT SALESMAN-NUMBER
           DISPLAY "Input salesman_Name: "
           ACCEPT SALESMAN-NAME
           PERFORM GET-SALES-ARGS.

       GET-SALES-ARGS.
           DISPLAY "Input unit_sold: "
           ACCEPT UNIT-SOLD
           DISPLAY "Input unit_price: "
           ACCEPT UNIT-PRICE.

       GET-DAY.
           DISPLAY "Input day number (1-7): "
           ACCEPT DAY-NUMBER.

       GET-LETTER.
           DISPLAY "Input a letter: "
           ACCEPT LETTER.

      *> ================================================================
      *> CALC-... PARAGRAPHS (the actual selection / decision logic)
      *> ================================================================
       CALC-AVERAGE.
           COMPUTE RET-NUM ROUNDED =
               (PRELIM + MIDTERM + FINAL-GRADE) / 3.

       CALC-POS-NEG.
           EVALUATE TRUE
               WHEN NUM-VALUE > 0
                   MOVE 1 TO RET-INT
               WHEN NUM-VALUE < 0
                   MOVE -1 TO RET-INT
               WHEN OTHER
                   MOVE 0 TO RET-INT
           END-EVALUATE.

       CALC-ODD-EVEN.
           EVALUATE TRUE
               WHEN INT-A = 0
                   MOVE 0 TO RET-INT
               WHEN FUNCTION MOD(INT-A, 2) = 0
                   MOVE 2 TO RET-INT
               WHEN OTHER
                   MOVE 1 TO RET-INT
           END-EVALUATE.

       CALC-LARGEST.
           IF INT-A >= INT-B
               MOVE INT-A TO RET-INT
           ELSE
               MOVE INT-B TO RET-INT
           END-IF.

       CALC-SMALLEST.
           MOVE INT-A TO RET-INT
           IF INT-B < RET-INT
               MOVE INT-B TO RET-INT
           END-IF
           IF INT-C < RET-INT
               MOVE INT-C TO RET-INT
           END-IF.

       CALC-EQUIV.
      *> RET-NUM = 0 means the average is invalid (below 0 or above 100)
           EVALUATE TRUE
               WHEN AVG-VALUE > 100
                   MOVE 0 TO RET-NUM
               WHEN AVG-VALUE < 0
                   MOVE 0 TO RET-NUM
               WHEN AVG-VALUE >= 97
                   MOVE 1.00 TO RET-NUM
               WHEN AVG-VALUE >= 94
                   MOVE 1.25 TO RET-NUM
               WHEN AVG-VALUE >= 91
                   MOVE 1.50 TO RET-NUM
               WHEN AVG-VALUE >= 88
                   MOVE 1.75 TO RET-NUM
               WHEN AVG-VALUE >= 86
                   MOVE 2.00 TO RET-NUM
               WHEN AVG-VALUE >= 82
                   MOVE 2.25 TO RET-NUM
               WHEN AVG-VALUE >= 79
                   MOVE 2.75 TO RET-NUM
               WHEN AVG-VALUE >= 75
                   MOVE 3.00 TO RET-NUM
               WHEN OTHER
                   MOVE 5.00 TO RET-NUM
           END-EVALUATE.

       CALC-TEMP.
           EVALUATE TRUE
               WHEN TEMP-VALUE < 0
                   MOVE 1 TO RET-INT
               WHEN TEMP-VALUE < 10
                   MOVE 2 TO RET-INT
               WHEN TEMP-VALUE < 20
                   MOVE 3 TO RET-INT
               WHEN TEMP-VALUE < 30
                   MOVE 4 TO RET-INT
               WHEN TEMP-VALUE < 40
                   MOVE 5 TO RET-INT
               WHEN OTHER
                   MOVE 6 TO RET-INT
           END-EVALUATE.

       CALC-SALES.
           COMPUTE RET-NUM ROUNDED = UNIT-SOLD * UNIT-PRICE.

       CALC-COMMISSION.
      *> Commission is based on the total sales stored in RET-NUM.
           EVALUATE TRUE
               WHEN RET-NUM <= 15000
                   COMPUTE COMMISSION ROUNDED = RET-NUM * 0.15
               WHEN RET-NUM <= 20000
                   COMPUTE COMMISSION ROUNDED = RET-NUM * 0.20
               WHEN RET-NUM <= 25000
                   COMPUTE COMMISSION ROUNDED = RET-NUM * 0.25
               WHEN RET-NUM <= 30000
                   COMPUTE COMMISSION ROUNDED = RET-NUM * 0.30
               WHEN OTHER
                   COMPUTE COMMISSION ROUNDED = RET-NUM * 0.40
           END-EVALUATE.

       CALC-DAY.
           IF DAY-NUMBER >= 1 AND DAY-NUMBER <= 7
               MOVE DAY-NUMBER TO RET-INT
           ELSE
               MOVE 0 TO RET-INT
           END-IF.

       CALC-LETTER.
           EVALUATE LETTER
               WHEN "a"
               WHEN "e"
               WHEN "i"
               WHEN "o"
               WHEN "u"
               WHEN "A"
               WHEN "E"
               WHEN "I"
               WHEN "O"
               WHEN "U"
                   MOVE "V" TO RET-CHAR
               WHEN OTHER
                   IF LETTER IS ALPHABETIC AND LETTER NOT = SPACE
                       MOVE "C" TO RET-CHAR
                   ELSE
                       MOVE "N" TO RET-CHAR
                   END-IF
           END-EVALUATE.

      *> ================================================================
      *> SHOW-... PARAGRAPHS (print the result from the stored values)
      *> ================================================================
       SHOW-AVERAGE.
           MOVE RET-NUM TO ED-DEC
           DISPLAY "Your average is: " FUNCTION TRIM(ED-DEC)
           IF RET-NUM >= 75
               DISPLAY "Passed"
           ELSE
               DISPLAY "Failed"
           END-IF.

       SHOW-POS-NEG.
           EVALUATE RET-INT
               WHEN 1
                   DISPLAY "Positive"
               WHEN -1
                   DISPLAY "Negative"
               WHEN OTHER
                   DISPLAY "Neutral"
           END-EVALUATE.

       SHOW-ODD-EVEN.
           EVALUATE RET-INT
               WHEN 0
                   DISPLAY "Neutral"
               WHEN 2
                   DISPLAY "Even"
               WHEN OTHER
                   DISPLAY "Odd"
           END-EVALUATE.

       SHOW-LARGEST.
           EVALUATE TRUE
               WHEN INT-A > INT-B
                   DISPLAY "num1 is greater than num2"
               WHEN INT-B > INT-A
                   DISPLAY "num2 is greater than num1"
               WHEN OTHER
                   DISPLAY "Both numbers are equal"
           END-EVALUATE
           MOVE RET-INT TO ED-INT
           DISPLAY "Largest: " FUNCTION TRIM(ED-INT).

       SHOW-SMALLEST.
           EVALUATE TRUE
               WHEN RET-INT = INT-A
                   DISPLAY "num1 is the smallest"
               WHEN RET-INT = INT-B
                   DISPLAY "num2 is the smallest"
               WHEN OTHER
                   DISPLAY "num3 is the smallest"
           END-EVALUATE
           MOVE RET-INT TO ED-INT
           DISPLAY "Smallest: " FUNCTION TRIM(ED-INT).

       SHOW-EQUIV.
           IF RET-NUM = 0
               DISPLAY "Invalid"
           ELSE
               MOVE RET-NUM TO ED-GRADE
               DISPLAY "Equivalent Grade: " ED-GRADE
           END-IF.

       SHOW-TEMP.
           EVALUATE RET-INT
               WHEN 1
                   DISPLAY "Freezing Weather"
               WHEN 2
                   DISPLAY "Very Cold Weather"
               WHEN 3
                   DISPLAY "Cold Weather"
               WHEN 4
                   DISPLAY "Normal temp"
               WHEN 5
                   DISPLAY "Hot"
               WHEN OTHER
                   DISPLAY "Very Hot"
           END-EVALUATE.

       SHOW-SALES.
           MOVE RET-NUM TO ED-DEC
           DISPLAY "Total Sales: " FUNCTION TRIM(ED-DEC)
           PERFORM CALC-COMMISSION
           MOVE COMMISSION TO ED-DEC
           DISPLAY "Commission: " FUNCTION TRIM(ED-DEC).

       SHOW-DAY.
           EVALUATE RET-INT
               WHEN 1
                   DISPLAY "Monday"
               WHEN 2
                   DISPLAY "Tuesday"
               WHEN 3
                   DISPLAY "Wednesday"
               WHEN 4
                   DISPLAY "Thursday"
               WHEN 5
                   DISPLAY "Friday"
               WHEN 6
                   DISPLAY "Saturday"
               WHEN 7
                   DISPLAY "Sunday"
               WHEN OTHER
                   DISPLAY "Invalid Day"
           END-EVALUATE.

       SHOW-LETTER.
           EVALUATE RET-CHAR
               WHEN "V"
                   DISPLAY "Vowel"
               WHEN "C"
                   DISPLAY "Consonant"
               WHEN OTHER
                   DISPLAY "Not a letter"
           END-EVALUATE.

      *> Used by the FUNCTION 3 menu to show the returned value.
       SHOW-RETURN-NUM.
           MOVE RET-NUM TO ED-DEC
           DISPLAY "Return value: " FUNCTION TRIM(ED-DEC).

       SHOW-RETURN-INT.
           MOVE RET-INT TO ED-INT
           DISPLAY "Return value: " FUNCTION TRIM(ED-INT).

       SHOW-RETURN-CHAR.
           DISPLAY "Return value: " RET-CHAR.
