<?php 
/**
 *The main index template file
**/
get_header(); ?>
<section class="section-main col-md-12 top-section">
	<div class="content-page">
		<div class="page-title col-md-12">
			<div class="container">
				<div class="row">
					<div class="col-md-6 col-sm-6 page-title-captions">
						<h1><?php $blogtitle_check = get_theme_mod( 'fulgent_blogtitle' );
								if( $blogtitle_check != '' ) {  
									echo esc_attr( get_theme_mod('fulgent_blogtitle', '') );
								 } else { 	
									echo __('Our Blog','fulgent');
							 } ?></h1>
					</div>
					<div class="col-md-6 col-sm-6 breadcrumbs">
						<ul>
							<?php fulgent_custom_breadcrumbs(); ?>
						</ul>
					</div>
				</div>
			</div>
		</div>
		<?php get_template_part( 'content' ); ?>
	</div>
</section>
<?php get_footer(); ?>
