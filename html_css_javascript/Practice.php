<?php
    $toemail = strval($_GET['to']);
    $from = (string)$_GET['from'];
    $subject = (string)$_GET['subject'];
    $message = (string)$_GET['message'];
    $headers = "MIME-Version: 1.0" . "\r\n";
    $headers .= "Content-type:text/html;charset=UTF-8" . "\r\n";
    $headers .= 'From: <' . $from . '>' . "\r\n";
    mail($toemail, $subject, $message, $headers);
?>

<!DOCTYPE html>
<html>
<head>
    <p>Message Sent <?php echo $toemail?></p>
</head>
</html>