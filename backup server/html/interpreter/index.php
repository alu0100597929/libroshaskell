<?php

function show_form($result = '', $code = '')
{
	include('static/form.html');
	die();
}

function execute_haskell($code)
{
	$code .= "\r\n";
	$file = sprintf('/tmp/%s.txt', md5($code));
	file_put_contents($file, $code);
	$file = escapeshellarg($file);
	$result = `../bin/interprete $file`;
	unlink($file);
	return $result;
}

switch($_SERVER['REQUEST_METHOD'])
{
	// Handle GET request
	case 'GET':
		show_form();
		break;

	// Handle POST request
	case 'POST':
		if(!isset($_POST['action']))
			show_form();
		switch($_POST['action'])
		{
			case 'file':
				if(!isset($_FILES['file']))
					show_form();
				$code = file_get_contents($_FILES['file']['tmp_name']);
				unlink($_FILES['file']['tmp_name']);
				$result = execute_haskell($code);
				show_form($result, $code);
				break;

			case 'code':
				if(!isset($_POST['code']))
					show_form();
				$result = execute_haskell($_POST['code']);
				show_form($result, $_POST['code']);
        		break;

			default:
				show_form();
				break;
		}
		break;

	// Handle unknown request method
	default:
		header('HTTP/1.1 405 Method Not Allowed', TRUE, 405);
		die('<h1>405 Method Not Allowed</h1>');
		break;
}
