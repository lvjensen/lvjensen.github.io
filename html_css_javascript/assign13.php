<!DOCTYPE html>
<html>
    <head>
        <?php
        $fname = $_GET['fname'];
        $lname = $_GET['lname'];
        $id = $_GET['id'];
        $skill = $_GET['skill'];
        $perf = $_GET['perf'];
        $building = $_GET['building'];
        $room = $_GET['room'];
        $time = $_GET['time'];
        $instrument = $_GET['instrument'];
        $fname2 = $_GET['fname2'];
        $lname2 = $_GET['lname2'];
        $id2 = $_GET['id2'];
        
        if ($fname2 == "") {
            $txt = "<table style='text-align:center;'><tr><td style='width:200px'>" . $fname . " " . $lname . "</td><td style='width:100px'>" . $id  . "</td><td style='width:100px'>" . $perf . "</td><td style='width:100px'>" . $skill . "</td><td style='width:100px'>" . $instrument . "</td><td style='width:100px'>" . $building . "</td><td style='width:100px'>" . $room . "</td><td style='width:100px'>" . $time  . "</td></table>"; 
        } else {
            $txt = "<table style='text-align:center;'><tr><td style='width:200px'>" . $fname . " " . $lname . "</td><td style='width:100px'>" . $id  . "</td><td style='width:100px'>" . $perf . "</td><td style='width:100px'>" . $skill . "</td><td style='width:100px'>" . $instrument . "</td><td style='width:100px'>" . $building . "</td><td style='width:100px'>" . $room . "</td><td style='width:100px'>" . $time  . "</td></table>" . "<table style='text-align:center;'><tr><td style='width:200px'>" . $fname2 . " " . $lname2 . "</td><td style='width:100px'>" . $id2  . "</td><td style='width:100px'>" . $perf . "</td><td style='width:100px'>" . $skill . "</td><td style='width:100px'>" . $instrument . "</td><td style='width:100px'>" . $building . "</td><td style='width:100px'>" . $room . "</td><td style='width:100px'>" . $time  . "</td></table>";             
            
        }
        
        $file1 = fopen("file1.txt", "a+") or die("Unable to open file!");
        fwrite($file1, $txt);
        fclose($file1);
        ?>
    </head>
    <body>
    </body>
</html>