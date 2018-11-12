# vizscorer
a clever bot to rate and improve your ggplot plot

Vizscorer is an R package which tries to help users discover weaknesses in their ggplot plots. To do so it applies a mix of machine learning and natural language generation.

<a href="http://www.andreacirillo.com/images/vizscorer_viz.001.png" ><figure class= "post_image" >
<img src="http://www.andreacirillo.com/images/vizscorer_viz.001.png"  alt="infoviz" style="width: 300px;"/>
</figure></a>

Vizscorer basically:

- looks at ggplot internals 
- analyzes them
- produces a deck of slides showing an overall evaluation and detailed suggestions about how to improve the plot.

See it in action below:

<a href="http://www.andreacirillo.com/images/vizscorer_demo.gif" ><figure class= "post_image" >
<img src="http://www.andreacirillo.com/images/vizscorer_demo.gif"  alt="infoviz" style="width: 300px;"/>
</figure></a>

Vizscorer continues the effort started with [paletteR](https://github.com/AndreaCirilloAC/paletter) to increase the level of quality of the average plot produced in companies, where there is no time to study data visualization theory.

### Disclaimer

**vizscorer is currently in beta mode and every feedback and support from early adopters is more than welcome :)**

### further area of development

- increasing the number of plots employed for training the gradient boosting model
- introducing a “console version” of the report, more usable and less annoying in the long run
- increasing the level of cleaverness of the bot to make it able to analyse more complex plots and provide even more useful suggestions