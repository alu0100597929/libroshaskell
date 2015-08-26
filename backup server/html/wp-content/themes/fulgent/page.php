<?php 
/**
 * Main Page template file
**/
get_header(); ?>
<section class="section-main col-md-12 top-section" id="post-<?php the_ID(); ?>" <?php post_class(); ?>>
	<div class="content-page">
		<div class="page-title col-md-12">
			<div class="container">
				<div class="row">
					<div class="col-md-6 col-sm-6 page-title-captions">
						<h1><?php the_title(); ?></h1>
					</div>
					<div class="col-md-6 col-sm-6 breadcrumbs">
						<ul>
							<?php if (function_exists('fulgent_custom_breadcrumbs')) fulgent_custom_breadcrumbs(); ?>
						</ul>
					</div>
				</div>
			</div>
		</div>
					
		<div class="theme-content page-margin-top col-md-12">
	<div class="container">
		<div class="row">
			<div class="content-blog col-md-8">
				 <?php while ( have_posts() ) : the_post(); ?>
						<div class="ourblog-box">
							<div class="post-header">
								<div class="post-date-blog">
									<span class="date"><a href="<?php echo esc_url(get_day_link(get_post_time('Y'), get_post_time('m'), get_post_time('j')));?>" ><?php echo get_the_date("F j, Y "); ?></a></span>
								</div>
								<?php if ( has_post_thumbnail() ) : ?>
									<div class="image-wrapper">
										<a href="<?php echo esc_url( get_permalink() ); ?>">
											<?php the_post_thumbnail( 'full', array( 'alt' => get_the_title(), 'class' => 'img-responsive') ); ?>
										</a>
									</div>
								<?php endif; ?>
							</div>
								<div class="post-detail">
									<?php 
										the_content();
													wp_link_pages( array(
													'before'      => '<div class="page-links"><span class="page-links-title">' . __( 'Pages:', 'fulgent' ) . '</span>',
													'after'       => '</div>',
													'link_before' => '<span>',
													'link_after'  => '</span>',
												) );
							  ?> 
							</div>
						</div>
				<?php endwhile;
					wp_reset_postdata()
				 ?>
				
			</div>  
			<?php get_sidebar(); ?>
		</div>
	</div>
</div>
		
	</div>
</section>
<?php get_footer(); ?>
