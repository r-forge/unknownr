<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="/"><img src="<?php echo $themeroot; ?>/images/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>

<h2>Welcome to the unknownR project!</h2> 
<p>Do you know how many functions there are in base R?<br>
How many of them do you know you don't know?<br>
Run unk() to discover your unknown unknowns.<br>
It's fast and it's fun!</p>

<p>The package was presented at LondonR March 2011 <a href="http://www.londonr.org/unknownR.pdf"<strong>here</strong></a>. This presentation serves as a quick introduction. It is strongly recommended to read this before running unk() for the first time.</p>

<p>Latest stable release: <a href="http://cran.r-project.org/package=data.table"><strong>on CRAN</strong></a>.</p>

<p>Please vote for the package <a href="http://crantastic.org/packages/unknownR"><strong>here</strong></a>.</p>

<p> The development summary page you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>.</p>
<!-- Start of StatCounter Code -->
<script type="text/javascript">
var sc_project=6700858; 
var sc_invisible=1; 
var sc_security="3e5c47ee"; 
</script>
<script type="text/javascript"
src="http://www.statcounter.com/counter/counter.js"></script>
<noscript><div class="statcounter"><a title="website
statistics" href="http://statcounter.com/free-web-stats/"
target="_blank"><img class="statcounter"
src="http://c.statcounter.com/6700858/0/3e5c47ee/1/"
alt="website statistics"></a></div></noscript>
<!-- End of StatCounter Code -->
</body>
</html>

