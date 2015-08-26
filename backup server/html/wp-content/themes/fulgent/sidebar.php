<?php 
/**
 * The left sidebar template file
**/
?>
<?php if(is_page_template('page-templates/left-sidebar.php')){ 
	$flugent_offset_class= '';
}else{
	$flugent_offset_class= 'col-md-offset-1';
}
?>

<div class="sidebar col-md-3 <?php echo $flugent_offset_class; ?> main-sidebar">
<?php if ( is_active_sidebar( 'sidebar-1' ) ) { 
			 dynamic_sidebar( 'sidebar-1' );
	 } ?>
</div>
