<!DOCTYPE html>
<html>
    <head>
        <?php
            $submission = $_GET['submission'];
            $label = '';
            if ($submission == 'Submit Order') {
                $label = 'Order Submitted';
            } else {
                $label = 'Order Canceled';
            }
        ?>
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
        <title>Order Status</title>
    </head>
    <body>
        <div id='review'>
            <h1><?php echo $label ?></h1>
        </div>
    </body>
</html>