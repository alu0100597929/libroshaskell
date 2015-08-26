<?php 
/**
 * The Search template file
**/
get_header(); ?>
<section class="section-main col-md-12 top-section">
	<div class="content-page">
		<div class="page-title col-md-12">
			<div class="container">
				<div class="row">
					<div class="col-md-6 col-sm-6 page-title-captions">
					  <h1><?php
					_e('Search results for', 'fulgent');
					echo " : " . get_search_query();
					?></h1>
					</div>
					<div class="col-md-6 col-sm-6 breadcrumbs">
						<ul>
							<?php fulgent_custom_breadcrumbs(); ?>
							
						</ul>
					</div>
				</div>
			</div>
		</div>
			
		<div class="theme-content page-margin-top col-md-12">
			<div class="container">
		<div class="row">
			<div class="content-blog col-md-8">
				 <?php if(have_posts()) : while ( have_posts() ) : the_post(); ?>
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
									<a href="<?php echo esc_url( get_permalink() ); ?>" class="post-title"><?php the_title_attribute(); ?></a>
									<?php fulgent_entry_meta(); ?>  
									<?php the_excerpt(); ?> 
								</div>
						</div>
				<?php endwhile; else: ?>
				<h3><?php _e('It looks like nothing was found at this location. Maybe try a search?','fulgent'); ?></h3>
				<?php get_search_form(); ?>
				<?php endif; ?>
					<div class="site-pagination">      
			<?php fulgent_pagination(); ?>
		</div>
			</div>  
			<?php get_sidebar(); ?>
		</div>
	</div>
		</div>	
    </div>    
</section>
<?php get_footer(); ?>
