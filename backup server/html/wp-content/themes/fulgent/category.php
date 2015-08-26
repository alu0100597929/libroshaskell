<?php 
/**
 * The category template file
**/
get_header(); 
?>
<section class="section-main col-md-12 top-section">
	<div class="content-page">
		<div class="page-title col-md-12">
			<div class="container">
				<div class="row">
					<div class="col-md-6 col-sm-6 page-title-captions">
					  <h1><?php echo esc_attr(single_cat_title('', false)); ?></h1>
					</div>
					<div class="col-md-6 col-sm-6 breadcrumbs">
						<ul>
							<?php fulgent_custom_breadcrumbs(); ?>
						</ul>
					</div>
				</div>
			</div>
		</div>
		<?php get_template_part( 'content'); ?>
	</div>
</section>
<?php get_footer(); ?>
