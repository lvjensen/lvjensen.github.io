<!DOCTYPE html>
<html>
    <head>
        <?php
            $first = $_GET["first"];
            $last = $_GET["last"];
            $address = $_GET["address"];
            $phone = $_GET["phone"];
            $total = $_GET['total'];
            $fullname = $first . ' ' . $last;
            $sales = '$' . $_GET['total'] . '.00';
            $expiration = $_GET['expiration'];
            $month = (int)substr($expiration, 0, 2);
            $monthlist = array('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December');
            $ctype = $_GET['ctype'];
            $ccnum = $_GET['ccnum'];
            $expyear = (int)substr($expiration, 3, 4);
        ?>
        <title>Order Review</title>
        <meta charset="utf-8">
        <style>
            #review{
                margin: 0px 200px 0px 200px;
                text-align: center;
                background-color: #e9e9e9;
                padding: 15px;
                border-radius: 5px;
                border: 2px solid black;
            }
            body {
                background-color: #99d3df;
            }
        </style>
    </head>
    <body>
        <div id = 'review'>
            <h1>Confirmation Page</h1>
            <p>Name: <?php echo $fullname ?></p>
            <p>Phone #: <?php echo $phone?></p>
            <p>Address: <?php echo $address?></p>
            <p>Total: <?php echo $sales?></p>
            <p>Credit Card Type: <?php echo $ctype ?></p>
            <p>Credit Card Number: <?php echo $ccnum ?></p>
            <p>Credit Card Expiration: <?php echo $monthlist[$month-1] . ' ' . $expyear ?></p>
            <form action='assign11_a.php' method="get">
                <input type='submit' value='Submit Order' name="submission" id='lit'>
                <input type='submit' value='Cancel Order' name='submission' id='cool'>
            </form>
        </div>
    </body>
</html>