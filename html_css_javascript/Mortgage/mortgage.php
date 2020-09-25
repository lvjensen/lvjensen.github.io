
<html>
<head>
    <title>Mortgage Output</title>
    <?php
        $apr = (int)$_GET["apr"];
        $term = (int)$_GET["term"];
        $amount = (int)$_GET["amount"];
        $newapr = $apr * .05;
        $newterm = $term * 12;
        #function total() {
        #    $monthly = (($amount * (($newapr * (1 + $newapr)**$newterm)))/((1 + $newapr)**$newterm - 1));
        #    print($monthly);
        #}
        $monthly = (int)$_GET["monthly"];
    ?>
    <style>
        body {
            background-color: #8eaebd;
        }
        
        header {
            background-color: #cf6766;
            font-size: 300%;
            text-align: center;
            font-family: 'Bad Script';
            
        }
        
        #lit {
            text-align: center;
            border: solid #031424 5px;
            border-radius: 5px;
            background-color: #30415d;
            padding: 20px;
            margin: 50px 350px 50px 350px;
        }
        
        p {
            font-size: 130%;
            background-color: darkgray;
            border: solid #031424 2px;
            padding: 10px;
            margin: 0px 100px 0px 100px;
        }
    </style>
</head>
<body>
    <div id='lit'>
        <p>APR: <?php echo $apr ?>%<br>
        Term: <?php echo $term?><br>
        Amount: $ <?php echo $amount?><br>
            <b><span style='font-size:160%;'>Monthly Payment: $<?php echo $monthly ?></span></b></p>
    </div>
    
</body>
</html>