/*
Holder.js - client side image placeholders
© 2012-2014 Ivan Malopinsky - http://imsky.co
*/
(function(register, global, undefined) {

	//Constants and definitions

	var SVG_NS = 'http://www.w3.org/2000/svg';
	var document = global.document;

	var Holder = {
		/**
		 * Adds a theme to default settings
		 *
		 * @param {string} name Theme name
		 * @param {Object} theme Theme object, with foreground, background, size, font, and fontweight properties.
		 */
		addTheme: function(name, theme) {
			name != null && theme != null && (App.settings.themes[name] = theme);
			delete App.vars.cache.themeKeys;
			return this;
		},

		/**
		 * Appends a placeholder to an element
		 *
		 * @param {string} src Placeholder URL string
		 * @param {string} el Selector of target element(s)
		 */
		addImage: function(src, el) {
			var node = document.querySelectorAll(el);
			if (node.length) {
				for (var i = 0, l = node.length; i < l; i++) {
					var img = newEl('img');
					setAttr(img, {
						'data-src': src
					});
					node[i].appendChild(img);
				}
			}
			return this;
		},

		/**
		 * Runs Holder with options. By default runs Holder on all images with "holder.js" in their source attributes.
		 *
		 * @param {Object} userOptions Options object, can contain domain, themes, images, and bgnodes properties
		 */
		run: function(userOptions) {
			userOptions = userOptions || {};
			var renderSettings = {};

			App.vars.preempted = true;

			var options = extend(App.settings, userOptions);

			renderSettings.renderer = options.renderer ? options.renderer : App.setup.renderer;
			if (App.setup.renderers.join(',').indexOf(renderSettings.renderer) === -1) {
				renderSettings.renderer = App.setup.supportsSVG ? 'svg' : (App.setup.supportsCanvas ? 'canvas' : 'html');
			}

			//< v2.4 API compatibility
			if (options.use_canvas) {
				renderSettings.renderer = 'canvas';
			} else if (options.use_svg) {
				renderSettings.renderer = 'svg';
			}

			var images = getNodeArray(options.images);
			var bgnodes = getNodeArray(options.bgnodes);
			var stylenodes = getNodeArray(options.stylenodes);
			var objects = getNodeArray(options.objects);

			renderSettings.stylesheets = [];
			renderSettings.svgXMLStylesheet = true;
			renderSettings.noFontFallback = options.noFontFallback ? options.noFontFallback : false;

			for (var i = 0; i < stylenodes.length; i++) {
				var styleNode = stylenodes[i];
				if (styleNode.attributes.rel && styleNode.attributes.href && styleNode.attributes.rel.value == 'stylesheet') {
					var href = styleNode.attributes.href.value;
					//todo: write isomorphic relative-to-absolute URL function
					var proxyLink = newEl('a');
					proxyLink.href = href;
					var stylesheetURL = proxyLink.protocol + '//' + proxyLink.host + proxyLink.pathname + proxyLink.search;
					renderSettings.stylesheets.push(stylesheetURL);
				}
			}

			for (i = 0; i < bgnodes.length; i++) {
				var backgroundImage = global.getComputedStyle(bgnodes[i], null).getPropertyValue('background-image');
				var dataBackgroundImage = bgnodes[i].getAttribute('data-background-src');
				var rawURL = null;

				if (dataBackgroundImage == null) {
					rawURL = backgroundImage;
				} else {
					rawURL = dataBackgroundImage;
				}

				var holderURL = null;
				var holderString = '?' + options.domain + '/';

				if (rawURL.indexOf(holderString) === 0) {
					holderURL = rawURL.slice(1);
				} else if (rawURL.indexOf(holderString) != -1) {
					var fragment = rawURL.substr(rawURL.indexOf(holderString)).slice(1);
					var fragmentMatch = fragment.match(/([^\"]*)"?\)/);

					if (fragmentMatch != null) {
						holderURL = fragmentMatch[1];
					}
				}

				if (holderURL != null) {
					var holderFlags = parseURL(holderURL, options);
					if (holderFlags) {
						prepareDOMElement('background', bgnodes[i], holderFlags, renderSettings);
					}
				}
			}

			for (i = 0; i < objects.length; i++) {
				var object = objects[i];
				var objectAttr = {};

				try {
					objectAttr.data = object.getAttribute('data');
					objectAttr.dataSrc = object.getAttribute('data-src');
				} catch (e) {}

				var objectHasSrcURL = objectAttr.data != null && objectAttr.data.indexOf(options.domain) === 0;
				var objectHasDataSrcURL = objectAttr.dataSrc != null && objectAttr.dataSrc.indexOf(options.domain) === 0;

				if (objectHasSrcURL) {
					prepareImageElement(options, renderSettings, objectAttr.data, object);
				} else if (objectHasDataSrcURL) {
					prepareImageElement(options, renderSettings, objectAttr.dataSrc, object);
				}
			}

			for (i = 0; i < images.length; i++) {
				var image = images[i];
				var imageAttr = {};

				try {
					imageAttr.src = image.getAttribute('src');
					imageAttr.dataSrc = image.getAttribute('data-src');
					imageAttr.rendered = image.getAttribute('data-holder-rendered');
				} catch (e) {}

				var imageHasSrc = imageAttr.src != null;
				var imageHasDataSrcURL = imageAttr.dataSrc != null && imageAttr.dataSrc.indexOf(options.domain) === 0;
				var imageRendered = imageAttr.rendered != null && imageAttr.rendered == 'true';

				if (imageHasSrc) {
					if (imageAttr.src.indexOf(options.domain) === 0) {
						prepareImageElement(options, renderSettings, imageAttr.src, image);
					} else if (imageHasDataSrcURL) {
						//Image has a valid data-src and an invalid src
						if (imageRendered) {
							//If the placeholder has already been render, re-render it
							prepareImageElement(options, renderSettings, imageAttr.dataSrc, image);
						} else {
							//If the placeholder has not been rendered, check if the image exists and render a fallback if it doesn't
              (function(src, options, renderSettings, dataSrc, image){
                imageExists(src, function(exists){
                  if(!exists){
                    prepareImageElement(options, renderSettings, dataSrc, image);
                  }
                });
              })(imageAttr.src, options, renderSettings, imageAttr.dataSrc, image);
						}
					}
				} else if (imageHasDataSrcURL) {
					prepareImageElement(options, renderSettings, imageAttr.dataSrc, image);
				}
			}

			return this;
		},
		//todo: remove invisibleErrorFn for 2.5
		invisibleErrorFn: function(fn) {
			return function(el) {
				if (el.hasAttribute('data-holder-invisible')) {
					throw 'Holder: invisible placeholder';
				}
			};
		}
	};

	//< v2.4 API compatibility

	Holder.add_theme = Holder.addTheme;
	Holder.add_image = Holder.addImage;
	Holder.invisible_error_fn = Holder.invisibleErrorFn;

	var App = {
		settings: {
			domain: 'holder.js',
			images: 'img',
			objects: 'object',
			bgnodes: 'body .holderjs',
			stylenodes: 'head link.holderjs',
			stylesheets: [],
			themes: {
				'gray': {
					background: '#EEEEEE',
					foreground: '#AAAAAA'
				},
				'social': {
					background: '#3a5a97',
					foreground: '#FFFFFF'
				},
				'industrial': {
					background: '#434A52',
					foreground: '#C2F200'
				},
				'sky': {
					background: '#0D8FDB',
					foreground: '#FFFFFF'
				},
				'vine': {
					background: '#39DBAC',
					foreground: '#1E292C'
				},
				'lava': {
					background: '#F8591A',
					foreground: '#1C2846'
				}
			}
		},
    defaults: {
      size: 10,
      units: 'pt',
      scale: 1/16
    },
		flags: {
			dimensions: {
				regex: /^(\d+)x(\d+)$/,
				output: function(val) {
					var exec = this.regex.exec(val);
					return {
						width: +exec[1],
						height: +exec[2]
					};
				}
			},
			fluid: {
				regex: /^([0-9]+%?)x([0-9]+%?)$/,
				output: function(val) {
					var exec = this.regex.exec(val);
					return {
						width: exec[1],
						height: exec[2]
					};
				}
			},
			colors: {
				regex: /(?:#|\^)([0-9a-f]{3,})\:(?:#|\^)([0-9a-f]{3,})/i,
				output: function(val) {
					var exec = this.regex.exec(val);
					return {
						foreground: '#' + exec[2],
						background: '#' + exec[1]
					};
				}
			},
			text: {
				regex: /text\:(.*)/,
				output: function(val) {
					return this.regex.exec(val)[1].replace('\\/', '/');
				}
			},
			font: {
				regex: /font\:(.*)/,
				output: function(val) {
					return this.regex.exec(val)[1];
				}
			},
			auto: {
				regex: /^auto$/
			},
			textmode: {
				regex: /textmode\:(.*)/,
				output: function(val) {
					return this.regex.exec(val)[1];
				}
			},
			random: {
				regex: /^random$/
			}
		}
	};

	/**
	 * Processes provided source attribute and sets up the appropriate rendering workflow
	 *
	 * @private
	 * @param options Instance options from Holder.run
	 * @param renderSettings Instance configuration
	 * @param src Image URL
	 * @param el Image DOM element
	 */
	function prepareImageElement(options, renderSettings, src, el) {
		var holderFlags = parseURL(src.substr(src.lastIndexOf(options.domain)), options);
		if (holderFlags) {
			prepareDOMElement(null, el, holderFlags, renderSettings);
		}
	}

	/**
	 * Processes a Holder URL and extracts flags
	 *
	 * @private
	 * @param url URL
	 * @param options Instance options from Holder.run
	 */
	function parseURL(url, options) {
		var ret = {
			theme: extend(App.settings.themes.gray, null),
			stylesheets: options.stylesheets,
			holderURL: []
		};
		var render = false;
		var vtab = String.fromCharCode(11);
		var flags = url.replace(/([^\\])\//g, '$1' + vtab).split(vtab);
		var uriRegex = /%[0-9a-f]{2}/gi;
		for (var fl = flags.length, j = 0; j < fl; j++) {
			var flag = flags[j];
			if (flag.match(uriRegex)) {
				try {
					flag = decodeURIComponent(flag);
				} catch (e) {
					flag = flags[j];
				}
			}

			var push = false;

			if (App.flags.dimensions.match(flag)) {
				render = true;
				ret.dimensions = App.flags.dimensions.output(flag);
				push = true;
			} else if (App.flags.fluid.match(flag)) {
				render = true;
				ret.dimensions = App.flags.fluid.output(flag);
				ret.fluid = true;
				push = true;
			} else if (App.flags.textmode.match(flag)) {
				ret.textmode = App.flags.textmode.output(flag);
				push = true;
			} else if (App.flags.colors.match(flag)) {
				var colors = App.flags.colors.output(flag);
				ret.theme = extend(ret.theme, colors);
				//todo: convert implicit theme use to a theme: flag
				push = true;
			} else if (options.themes[flag]) {
				//If a theme is specified, it will override custom colors
				if (options.themes.hasOwnProperty(flag)) {
					ret.theme = extend(options.themes[flag], null);
				}
				push = true;
			} else if (App.flags.font.match(flag)) {
				ret.font = App.flags.font.output(flag);
				push = true;
			} else if (App.flags.auto.match(flag)) {
				ret.auto = true;
				push = true;
			} else if (App.flags.text.match(flag)) {
				ret.text = App.flags.text.output(flag);
				push = true;
			} else if (App.flags.random.match(flag)) {
				if (App.vars.cache.themeKeys == null) {
					App.vars.cache.themeKeys = Object.keys(options.themes);
				}
				var theme = App.vars.cache.themeKeys[0 | Math.random() * App.vars.cache.themeKeys.length];
				ret.theme = extend(options.themes[theme], null);
				push = true;
			}

			if (push) {
				ret.holderURL.push(flag);
			}
		}
		ret.holderURL.unshift(options.domain);
		ret.holderURL = ret.holderURL.join('/');
		return render ? ret : false;
	}

	/**
	 * Modifies the DOM to fit placeholders and sets up resizable image callbacks (for fluid and automatically sized placeholders)
	 *
	 * @private
	 * @param el Image DOM element
	 * @param flags Placeholder-specific configuration
	 * @param _renderSettings Instance configuration
	 */
	function prepareDOMElement(mode, el, flags, _renderSettings) {
		var dimensions = flags.dimensions,
			theme = flags.theme;
		var dimensionsCaption = dimensions.width + 'x' + dimensions.height;
		mode = mode == null ? (flags.fluid ? 'fluid' : 'image') : mode;

		if (flags.text != null) {
			theme.text = flags.text;

			//<object> SVG embedding doesn't parse Unicode properly
			if (el.nodeName.toLowerCase() === 'object') {
				var textLines = theme.text.split('\\n');
				for (var k = 0; k < textLines.length; k++) {
					textLines[k] = encodeHtmlEntity(textLines[k]);
				}
				theme.text = textLines.join('\\n');
			}
		}

		var holderURL = flags.holderURL;
		var renderSettings = extend(_renderSettings, null);

		if (flags.font) {
			theme.font = flags.font;
			//Only run the <canvas> webfont fallback if noFontFallback is false, if the node is not an image, and if canvas is supported
			if (!renderSettings.noFontFallback && el.nodeName.toLowerCase() === 'img' && App.setup.supportsCanvas && renderSettings.renderer === 'svg') {
				renderSettings = extend(renderSettings, {
					renderer: 'canvas'
				});
			}
		}

		//Chrome and Opera require a quick 10ms re-render if web fonts are used with canvas
		if (flags.font && renderSettings.renderer == 'canvas') {
			renderSettings.reRender = true;
		}

		if (mode == 'background') {
			if (el.getAttribute('data-background-src') == null) {
				setAttr(el, {
					'data-background-src': holderURL
				});
			}
		} else {
			setAttr(el, {
				'data-src': holderURL
			});
		}

		flags.theme = theme;

		el.holderData = {
			flags: flags,
			renderSettings: renderSettings
		};

		if (mode == 'image' || mode == 'fluid') {
			setAttr(el, {
				'alt': (theme.text ? (theme.text.length > 16 ? theme.text.substring(0, 16) + '…' : theme.text) + ' [' + dimensionsCaption + ']' : dimensionsCaption)
			});
		}

		if (mode == 'image') {
			if (renderSettings.renderer == 'html' || !flags.auto) {
				el.style.width = dimensions.width + 'px';
				el.style.height = dimensions.height + 'px';
			}
			if (renderSettings.renderer == 'html') {
				el.style.backgroundColor = theme.background;
			} else {
				render(mode, {
					dimensions: dimensions,
					theme: theme,
					flags: flags
				}, el, renderSettings);

				if (flags.textmode && flags.textmode == 'exact') {
					App.vars.resizableImages.push(el);
					updateResizableElements(el);
				}
			}
		} else if (mode == 'background' && renderSettings.renderer != 'html') {
			render(mode, {
					dimensions: dimensions,
					theme: theme,
					flags: flags
				},
				el, renderSettings);
		} else if (mode == 'fluid') {
			if (dimensions.height.slice(-1) == '%') {
				el.style.height = dimensions.height;
			} else if (flags.auto == null || !flags.auto) {
				el.style.height = dimensions.height + 'px';
			}
			if (dimensions.width.slice(-1) == '%') {
				el.style.width = dimensions.width;
			} else if (flags.auto == null || !flags.auto) {
				el.style.width = dimensions.width + 'px';
			}
			if (el.style.display == 'inline' || el.style.display === '' || el.style.display == 'none') {
				el.style.display = 'block';
			}

			setInitialDimensions(el);

			if (renderSettings.renderer == 'html') {
				el.style.backgroundColor = theme.background;
			} else {
				App.vars.resizableImages.push(el);
				updateResizableElements(el);
			}
		}
	}

	/**
	 * Core function that takes output from renderers and sets it as the source or background-image of the target element
	 *
	 * @private
	 * @param mode Placeholder mode, either background or image
	 * @param params Placeholder-specific parameters
	 * @param el Image DOM element
	 * @param renderSettings Instance configuration
	 */

	function render(mode, params, el, renderSettings) {
		var image = null;

		switch (renderSettings.renderer) {
			case 'svg':
				if (!App.setup.supportsSVG) return;
				break;
			case 'canvas':
				if (!App.setup.supportsCanvas) return;
				break;
			default:
				return;
		}

		//todo: move generation of scene up to flag generation to reduce extra object creation
		var scene = {
			width: params.dimensions.width,
			height: params.dimensions.height,
			theme: params.theme,
			flags: params.flags
		};

		var sceneGraph = buildSceneGraph(scene);

		var rendererParams = {
			text: scene.text,
			width: scene.width,
			height: scene.height,
			textHeight: scene.font.size,
			font: scene.font.family,
			fontWeight: scene.font.weight,
			template: scene.theme
		};

		function getRenderedImage() {
			var image = null;
			switch (renderSettings.renderer) {
				case 'canvas':
					image = sgCanvasRenderer(sceneGraph);
					break;
				case 'svg':
					image = sgSVGRenderer(sceneGraph, renderSettings);
					break;
				default:
					throw 'Holder: invalid renderer: ' + renderSettings.renderer;
			}
			return image;
		}

		image = getRenderedImage();

		if (image == null) {
			throw 'Holder: couldn\'t render placeholder';
		}

		//todo: add <object> canvas rendering
		if (mode == 'background') {
			el.style.backgroundImage = 'url(' + image + ')';
			el.style.backgroundSize = scene.width + 'px ' + scene.height + 'px';
		} else {
			if (el.nodeName.toLowerCase() === 'img') {
				setAttr(el, {
					'src': image
				});
			} else if (el.nodeName.toLowerCase() === 'object') {
				setAttr(el, {
					'data': image
				});
				setAttr(el, {
					'type': 'image/svg+xml'
				});
			}
			if (renderSettings.reRender) {
				setTimeout(function() {
					var image = getRenderedImage();
					if (image == null) {
						throw 'Holder: couldn\'t render placeholder';
					}
					if (el.nodeName.toLowerCase() === 'img') {
						setAttr(el, {
							'src': image
						});
					} else if (el.nodeName.toLowerCase() === 'object') {
						setAttr(el, {
							'data': image
						});
						setAttr(el, {
							'type': 'image/svg+xml'
						});
					}
				}, 100);
			}
		}
		setAttr(el, {
			'data-holder-rendered': true
		});
	}

	/**
	 * Core function that takes a Holder scene description and builds a scene graph
	 *
	 * @private
	 * @param scene Holder scene object
	 */
	function buildSceneGraph(scene) {
		scene.font = {
			family: scene.theme.font ? scene.theme.font : 'Arial, Helvetica, Open Sans, sans-serif',
			size: textSize(scene.width, scene.height, scene.theme.size ? scene.theme.size : App.defaults.size),
      units: scene.theme.units ? scene.theme.units : App.defaults.units,
			weight: scene.theme.fontweight ? scene.theme.fontweight : 'bold'
		};
		scene.text = scene.theme.text ? scene.theme.text : Math.floor(scene.width) + 'x' + Math.floor(scene.height);

		switch (scene.flags.textmode) {
			case 'literal':
				scene.text = scene.flags.dimensions.width + 'x' + scene.flags.dimensions.height;
				break;
			case 'exact':
				if (!scene.flags.exactDimensions) break;
				scene.text = Math.floor(scene.flags.exactDimensions.width) + 'x' + Math.floor(scene.flags.exactDimensions.height);
				break;
		}

		var sceneGraph = new SceneGraph({
			width: scene.width,
			height: scene.height
		});

		var Shape = sceneGraph.Shape;

		var holderBg = new Shape.Rect('holderBg', {
			fill: scene.theme.background
		});

		holderBg.resize(scene.width, scene.height);
		sceneGraph.root.add(holderBg);

		var holderTextGroup = new Shape.Group('holderTextGroup', {
			text: scene.text,
			align: 'center',
			font: scene.font,
			fill: scene.theme.foreground
		});

		holderTextGroup.moveTo(null, null, 1);
		sceneGraph.root.add(holderTextGroup);

		var tpdata = holderTextGroup.textPositionData = stagingRenderer(sceneGraph);
		if (!tpdata) {
			throw 'Holder: staging fallback not supported yet.';
		}
		holderTextGroup.properties.leading = tpdata.boundingBox.height;

		//todo: alignment: TL, TC, TR, CL, CR, BL, BC, BR
		var textNode = null;
		var line = null;

		function finalizeLine(parent, line, width, height) {
			line.width = width;
			line.height = height;
			parent.width = Math.max(parent.width, line.width);
			parent.height += line.height;
			parent.add(line);
		}

		if (tpdata.lineCount > 1) {
			var offsetX = 0;
			var offsetY = 0;
			var maxLineWidth = scene.width * App.setup.lineWrapRatio;
			var lineIndex = 0;
			line = new Shape.Group('line' + lineIndex);

			for (var i = 0; i < tpdata.words.length; i++) {
				var word = tpdata.words[i];
				textNode = new Shape.Text(word.text);
				var newline = word.text == '\\n';
				if (offsetX + word.width >= maxLineWidth || newline === true) {
					finalizeLine(holderTextGroup, line, offsetX, holderTextGroup.properties.leading);
					offsetX = 0;
					offsetY += holderTextGroup.properties.leading;
					lineIndex += 1;
					line = new Shape.Group('line' + lineIndex);
					line.y = offsetY;
				}
				if (newline === true) {
					continue;
				}
				textNode.moveTo(offsetX, 0);
				offsetX += tpdata.spaceWidth + word.width;
				line.add(textNode);
			}

			finalizeLine(holderTextGroup, line, offsetX, holderTextGroup.properties.leading);

			for (var lineKey in holderTextGroup.children) {
				line = holderTextGroup.children[lineKey];
				line.moveTo(
					(holderTextGroup.width - line.width) / 2,
					null,
					null);
			}

			holderTextGroup.moveTo(
				(scene.width - holderTextGroup.width) / 2, (scene.height - holderTextGroup.height) / 2,
				null);

			//If the text exceeds vertical space, move it down so the first line is visible
			if ((scene.height - holderTextGroup.height) / 2 < 0) {
				holderTextGroup.moveTo(null, 0, null);
			}
		} else {
			textNode = new Shape.Text(scene.text);
			line = new Shape.Group('line0');
			line.add(textNode);
			holderTextGroup.add(line);

			holderTextGroup.moveTo(
				(scene.width - tpdata.boundingBox.width) / 2, (scene.height - tpdata.boundingBox.height) / 2,
				null);
		}

		//todo: renderlist

		return sceneGraph;
	}

	/**
	 * Adaptive text sizing function
	 *
	 * @private
	 * @param width Parent width
	 * @param height Parent height
	 * @param fontSize Requested text size
	 */
	function textSize(width, height, fontSize) {
		height = parseInt(height, 10);
		width = parseInt(width, 10);
		var bigSide = Math.max(height, width);
		var smallSide = Math.min(height, width);
		var scale = App.defaults.scale;
		var newHeight = Math.min(smallSide * 0.75, 0.75 * bigSide * scale);
		return Math.round(Math.max(fontSize, newHeight));
	}

	/**
	 * Iterates over resizable (fluid or auto) placeholders and renders them
	 *
	 * @private
	 * @param element Optional element selector, specified only if a specific element needs to be re-rendered
	 */
	function updateResizableElements(element) {
		var images;
		if (element == null || element.nodeType == null) {
			images = App.vars.resizableImages;
		} else {
			images = [element];
		}
		for (var i in images) {
			if (!images.hasOwnProperty(i)) {
				continue;
			}
			var el = images[i];
			if (el.holderData) {
				var flags = el.holderData.flags;
				var dimensions = dimensionCheck(el, Holder.invisibleErrorFn(updateResizableElements));
				if (dimensions) {
					if (flags.fluid && flags.auto) {
						var fluidConfig = el.holderData.fluidConfig;
						switch (fluidConfig.mode) {
							case 'width':
								dimensions.height = dimensions.width / fluidConfig.ratio;
								break;
							case 'height':
								dimensions.width = dimensions.height * fluidConfig.ratio;
								break;
						}
					}

					var drawParams = {
						dimensions: dimensions,
						theme: flags.theme,
						flags: flags
					};

					if (flags.textmode && flags.textmode == 'exact') {
						flags.exactDimensions = dimensions;
						drawParams.dimensions = flags.dimensions;
					}

					render('image', drawParams, el, el.holderData.renderSettings);
				}
			}
		}
	}

	/**
	 * Checks if an element is visible
	 *
	 * @private
	 * @param el DOM element
	 * @param callback Callback function executed if the element is invisible
	 */
	function dimensionCheck(el, callback) {
		var dimensions = {
			height: el.clientHeight,
			width: el.clientWidth
		};
		if (!dimensions.height && !dimensions.width) {
			setAttr(el, {
				'data-holder-invisible': true
			});
			callback.call(this, el);
		} else {
			el.removeAttribute('data-holder-invisible');
			return dimensions;
		}
	}

	/**
	 * Sets up aspect ratio metadata for fluid placeholders, in order to preserve proportions when resizing
	 *
	 * @private
	 * @param el Image DOM element
	 */
	function setInitialDimensions(el) {
		if (el.holderData) {
			var dimensions = dimensionCheck(el, Holder.invisibleErrorFn(setInitialDimensions));
			if (dimensions) {
				var flags = el.holderData.flags;

				var fluidConfig = {
					fluidHeight: flags.dimensions.height.slice(-1) == '%',
					fluidWidth: flags.dimensions.width.slice(-1) == '%',
					mode: null,
					initialDimensions: dimensions
				};

				if (fluidConfig.fluidWidth && !fluidConfig.fluidHeight) {
					fluidConfig.mode = 'width';
					fluidConfig.ratio = fluidConfig.initialDimensions.width / parseFloat(flags.dimensions.height);
				} else if (!fluidConfig.fluidWidth && fluidConfig.fluidHeight) {
					fluidConfig.mode = 'height';
					fluidConfig.ratio = parseFloat(flags.dimensions.width) / fluidConfig.initialDimensions.height;
				}

				el.holderData.fluidConfig = fluidConfig;
			}
		}
	}

	//todo: see if possible to convert stagingRenderer to use HTML only
	var stagingRenderer = (function() {
		var svg = null,
			stagingText = null,
			stagingTextNode = null;
		return function(graph) {
			var rootNode = graph.root;
			if (App.setup.supportsSVG) {
				var firstTimeSetup = false;
				var tnode = function(text) {
					return document.createTextNode(text);
				};
				if (svg == null) {
					firstTimeSetup = true;
				}
				svg = initSVG(svg, rootNode.properties.width, rootNode.properties.height);
				if (firstTimeSetup) {
					stagingText = newEl('text', SVG_NS);
					stagingTextNode = tnode(null);
					setAttr(stagingText, {
						x: 0
					});
					stagingText.appendChild(stagingTextNode);
					svg.appendChild(stagingText);
					document.body.appendChild(svg);
					svg.style.visibility = 'hidden';
					svg.style.position = 'absolute';
					svg.style.top = '-100%';
					svg.style.left = '-100%';
					//todo: workaround for zero-dimension <svg> tag in Opera 12
					//svg.setAttribute('width', 0);
					//svg.setAttribute('height', 0);
				}

				var holderTextGroup = rootNode.children.holderTextGroup;
				var htgProps = holderTextGroup.properties;
				setAttr(stagingText, {
					'y': htgProps.font.size,
					'style': cssProps({
						'font-weight': htgProps.font.weight,
						'font-size': htgProps.font.size + htgProps.font.units,
						'font-family': htgProps.font.family,
						'dominant-baseline': 'middle'
					})
				});

				//Get bounding box for the whole string (total width and height)
				stagingTextNode.nodeValue = htgProps.text;
				var stagingTextBBox = stagingText.getBBox();

				//Get line count and split the string into words
				var lineCount = Math.ceil(stagingTextBBox.width / (rootNode.properties.width * App.setup.lineWrapRatio));
				var words = htgProps.text.split(' ');
				var newlines = htgProps.text.match(/\\n/g);
				lineCount += newlines == null ? 0 : newlines.length;

				//Get bounding box for the string with spaces removed
				stagingTextNode.nodeValue = htgProps.text.replace(/[ ]+/g, '');
				var computedNoSpaceLength = stagingText.getComputedTextLength();

				//Compute average space width
				var diffLength = stagingTextBBox.width - computedNoSpaceLength;
				var spaceWidth = Math.round(diffLength / Math.max(1, words.length - 1));

				//Get widths for every word with space only if there is more than one line
				var wordWidths = [];
				if (lineCount > 1) {
					stagingTextNode.nodeValue = '';
					for (var i = 0; i < words.length; i++) {
						if (words[i].length === 0) continue;
						stagingTextNode.nodeValue = decodeHtmlEntity(words[i]);
						var bbox = stagingText.getBBox();
						wordWidths.push({
							text: words[i],
							width: bbox.width
						});
					}
				}

				return {
					spaceWidth: spaceWidth,
					lineCount: lineCount,
					boundingBox: stagingTextBBox,
					words: wordWidths
				};
			} else {
				//todo: canvas fallback for measuring text on android 2.3
				return false;
			}
		};
	})();

	var sgCanvasRenderer = (function() {
		var canvas = newEl('canvas');
		var ctx = null;

		return function(sceneGraph) {
			if (ctx == null) {
				ctx = canvas.getContext('2d');
			}
			var root = sceneGraph.root;
			canvas.width = App.dpr(root.properties.width);
			canvas.height = App.dpr(root.properties.height);
			ctx.textBaseline = 'middle';

			ctx.fillStyle = root.children.holderBg.properties.fill;
			ctx.fillRect(0, 0, App.dpr(root.children.holderBg.width), App.dpr(root.children.holderBg.height));

			var textGroup = root.children.holderTextGroup;
			var tgProps = textGroup.properties;
			ctx.font = textGroup.properties.font.weight + ' ' + App.dpr(textGroup.properties.font.size) + textGroup.properties.font.units + ' ' + textGroup.properties.font.family + ', monospace';
			ctx.fillStyle = textGroup.properties.fill;

			for (var lineKey in textGroup.children) {
				var line = textGroup.children[lineKey];
				for (var wordKey in line.children) {
					var word = line.children[wordKey];
					var x = App.dpr(textGroup.x + line.x + word.x);
					var y = App.dpr(textGroup.y + line.y + word.y + (textGroup.properties.leading / 2));

					ctx.fillText(word.properties.text, x, y);
				}
			}

			return canvas.toDataURL('image/png');
		};
	})();

	var sgSVGRenderer = (function() {
		//Prevent IE <9 from initializing SVG renderer
		if (!global.XMLSerializer) return;
		var svg = initSVG(null, 0, 0);
		var bgEl = newEl('rect', SVG_NS);
		svg.appendChild(bgEl);

		//todo: create a reusable pool for textNodes, resize if more words present

		return function(sceneGraph, renderSettings) {
			var root = sceneGraph.root;

			initSVG(svg, root.properties.width, root.properties.height);
			var groups = svg.querySelectorAll('g');

			for (var i = 0; i < groups.length; i++) {
				groups[i].parentNode.removeChild(groups[i]);
			}

			setAttr(bgEl, {
				'width': root.children.holderBg.width,
				'height': root.children.holderBg.height,
				'fill': root.children.holderBg.properties.fill
			});

			var textGroup = root.children.holderTextGroup;
			var tgProps = textGroup.properties;
			var textGroupEl = newEl('g', SVG_NS);
			svg.appendChild(textGroupEl);

			for (var lineKey in textGroup.children) {
				var line = textGroup.children[lineKey];
				for (var wordKey in line.children) {
					var word = line.children[wordKey];
					var x = textGroup.x + line.x + word.x;
					var y = textGroup.y + line.y + word.y + (textGroup.properties.leading / 2);

					var textEl = newEl('text', SVG_NS);
					var textNode = document.createTextNode(null);

					setAttr(textEl, {
						'x': x,
						'y': y,
						'style': cssProps({
							'fill': tgProps.fill,
							'font-weight': tgProps.font.weight,
							'font-family': tgProps.font.family + ', monospace',
							'font-size': tgProps.font.size + tgProps.font.units,
							'dominant-baseline': 'central'
						})
					});

					textNode.nodeValue = word.properties.text;
					textEl.appendChild(textNode);
					textGroupEl.appendChild(textEl);
				}
			}

			var svgString = 'data:image/svg+xml;base64,' +
				btoa(unescape(encodeURIComponent(serializeSVG(svg, renderSettings))));
			return svgString;
		};
	})();

	//Helpers

	/**
	 * Generic new DOM element function
	 *
	 * @private
	 * @param tag Tag to create
	 * @param namespace Optional namespace value
	 */
	function newEl(tag, namespace) {
		if (namespace == null) {
			return document.createElement(tag);
		} else {
			return document.createElementNS(namespace, tag);
		}
	}

	/**
	 * Generic setAttribute function
	 *
	 * @private
	 * @param el Reference to DOM element
	 * @param attrs Object with attribute keys and values
	 */
	function setAttr(el, attrs) {
		for (var a in attrs) {
			el.setAttribute(a, attrs[a]);
		}
	}

	/**
	 * Generic SVG element creation function
	 *
	 * @private
	 * @param svg SVG context, set to null if new
	 * @param width Document width
	 * @param height Document height
	 */
	function initSVG(svg, width, height) {
		if (svg == null) {
			svg = newEl('svg', SVG_NS);
			var defs = newEl('defs', SVG_NS);
			svg.appendChild(defs);
		}
		//IE throws an exception if this is set and Chrome requires it to be set
		if (svg.webkitMatchesSelector) {
			svg.setAttribute('xmlns', SVG_NS);
		}

		setAttr(svg, {
			'width': width,
			'height': height,
			'viewBox': '0 0 ' + width + ' ' + height,
			'preserveAspectRatio': 'none'
		});
		return svg;
	}

	/**
	 * Generic SVG serialization function
	 *
	 * @private
	 * @param svg SVG context
	 * @param stylesheets CSS stylesheets to include
	 */
	function serializeSVG(svg, renderSettings) {
		if (!global.XMLSerializer) return;
		var serializer = new XMLSerializer();
		var svgCSS = '';
		var stylesheets = renderSettings.stylesheets;
		var defs = svg.querySelector('defs');

		//External stylesheets: Processing Instruction method
		if (renderSettings.svgXMLStylesheet) {
			var xml = new DOMParser().parseFromString('<xml />', 'application/xml');
			//Add <?xml-stylesheet ?> directives
			for (var i = stylesheets.length - 1; i >= 0; i--) {
				var csspi = xml.createProcessingInstruction('xml-stylesheet', 'href="' + stylesheets[i] + '" rel="stylesheet"');
				xml.insertBefore(csspi, xml.firstChild);
			}

			//Add <?xml ... ?> UTF-8 directive
			var xmlpi = xml.createProcessingInstruction('xml', 'version="1.0" encoding="UTF-8" standalone="yes"');
			xml.insertBefore(xmlpi, xml.firstChild);
			xml.removeChild(xml.documentElement);
			svgCSS = serializer.serializeToString(xml);
		}

		/*

		//External stylesheets: <link> method
		if (renderSettings.svgLinkStylesheet) {

			defs.removeChild(defs.firstChild);
			for (i = 0; i < stylesheets.length; i++) {
				var link = document.createElementNS('http://www.w3.org/1999/xhtml', 'link');
				link.setAttribute('href', stylesheets[i]);
				link.setAttribute('rel', 'stylesheet');
				link.setAttribute('type', 'text/css');
				defs.appendChild(link);
			}
		}

		//External stylesheets: <style> and @import method
		if (renderSettings.svgImportStylesheet) {
			var style = document.createElementNS(SVG_NS, 'style');
			var styleText = [];

			for (i = 0; i < stylesheets.length; i++) {
				styleText.push('@import url(' + stylesheets[i] + ');');
			}

			var styleTextNode = document.createTextNode(styleText.join('\n'));
			style.appendChild(styleTextNode);
			defs.appendChild(style);
		}

		*/

		var svgText = serializer.serializeToString(svg);
		svgText = svgText.replace(/\&amp;(\#[0-9]{2,}\;)/g, '&$1');
		return svgCSS + svgText;
	}

	/**
	 * Shallow object clone and merge
	 *
	 * @param a Object A
	 * @param b Object B
	 * @returns {Object} New object with all of A's properties, and all of B's properties, overwriting A's properties
	 */
	function extend(a, b) {
		var c = {};
		for (var x in a) {
			if (a.hasOwnProperty(x)) {
				c[x] = a[x];
			}
		}
		if (b != null) {
			for (var y in b) {
				if (b.hasOwnProperty(y)) {
					c[y] = b[y];
				}
			}
		}
		return c;
	}

	/**
	 * Takes a k/v list of CSS properties and returns a rule
	 *
	 * @param props CSS properties object
	 */
	function cssProps(props) {
		var ret = [];
		for (var p in props) {
			if (props.hasOwnProperty(p)) {
				ret.push(p + ':' + props[p]);
			}
		}
		return ret.join(';');
	}

	/**
	 * Prevents a function from being called too often, waits until a timer elapses to call it again
	 *
	 * @param fn Function to call
	 */
	function debounce(fn) {
		if (!App.vars.debounceTimer) fn.call(this);
		if (App.vars.debounceTimer) clearTimeout(App.vars.debounceTimer);
		App.vars.debounceTimer = setTimeout(function() {
			App.vars.debounceTimer = null;
			fn.call(this);
		}, App.setup.debounce);
	}

	/**
	 * Holder-specific resize/orientation change callback, debounced to prevent excessive execution
	 */
	function resizeEvent() {
		debounce(function() {
			updateResizableElements(null);
		});
	}

	/**
	 * Converts a value into an array of DOM nodes
	 *
	 * @param val A string, a NodeList, a Node, or an HTMLCollection
	 */
	function getNodeArray(val) {
		var retval = null;
		if (typeof(val) == 'string') {
			retval = document.querySelectorAll(val);
		} else if (global.NodeList && val instanceof global.NodeList) {
			retval = val;
		} else if (global.Node && val instanceof global.Node) {
			retval = [val];
		} else if (global.HTMLCollection && val instanceof global.HTMLCollection) {
			retval = val;
		} else if (val === null) {
			retval = [];
		}
		return retval;
	}

	/**
	 * Checks if an image exists
	 *
	 * @param params Configuration object, must specify at least a src key
	 * @param callback Callback to call once image status has been found
	 */
	function imageExists(src, callback) {
		var image = new Image();
		image.onerror = function() {
			callback.call(this, false);
		};
		image.onload = function() {
			callback.call(this, true);
		};
		image.src = src;
	}

	/**
	 * Encodes HTML entities in a string
	 *
	 * @param str Input string
	 */
	function encodeHtmlEntity(str) {
		var buf = [];
		var charCode = 0;
		for (var i = str.length - 1; i >= 0; i--) {
			charCode = str.charCodeAt(i);
			if (charCode > 128) {
				buf.unshift(['&#', charCode, ';'].join(''));
			} else {
				buf.unshift(str[i]);
			}
		}
		return buf.join('');
	}

	/**
	 * Decodes HTML entities in a stirng
	 *
	 * @param str Input string
	 */
	function decodeHtmlEntity(str) {
		return str.replace(/&#(\d+);/g, function(match, dec) {
			return String.fromCharCode(dec);
		});
	}

	// Scene graph

	var SceneGraph = function(sceneProperties) {
		var nodeCount = 1;

		//todo: move merge to helpers section
		function merge(parent, child) {
			for (var prop in child) {
				parent[prop] = child[prop];
			}
			return parent;
		}

		var SceneNode = augment.defclass({
			constructor: function(name) {
				nodeCount++;
				this.parent = null;
				this.children = {};
				this.id = nodeCount;
				this.name = 'n' + nodeCount;
				if (name != null) {
					this.name = name;
				}
				this.x = 0;
				this.y = 0;
				this.z = 0;
				this.width = 0;
				this.height = 0;
			},
			resize: function(width, height) {
				if (width != null) {
					this.width = width;
				}
				if (height != null) {
					this.height = height;
				}
			},
			moveTo: function(x, y, z) {
				this.x = x != null ? x : this.x;
				this.y = y != null ? y : this.y;
				this.z = z != null ? z : this.z;
			},
			add: function(child) {
					var name = child.name;
					if (this.children[name] == null) {
						this.children[name] = child;
						child.parent = this;
					} else {
						throw 'SceneGraph: child with that name already exists: ' + name;
					}
				}
				/*,	// probably unnecessary in Holder
				remove: function(name){
					if(this.children[name] == null){
						throw 'SceneGraph: child with that name doesn\'t exist: '+name;
					}
					else{
						child.parent = null;
						delete this.children[name];
					}
				},
				removeAll: function(){
					for(var child in this.children){
						this.remove(child);
					}
				}*/
		});

		var RootNode = augment(SceneNode, function(uber) {
			this.constructor = function() {
				uber.constructor.call(this, 'root');
				this.properties = sceneProperties;
			};
		});

		var Shape = augment(SceneNode, function(uber) {
			function constructor(name, props) {
				uber.constructor.call(this, name);
				this.properties = {
					fill: '#000'
				};
				if (props != null) {
					merge(this.properties, props);
				} else if (name != null && typeof name !== 'string') {
					throw 'SceneGraph: invalid node name';
				}
			}

			this.Group = augment.extend(this, {
				constructor: constructor,
				type: 'group'
			});

			this.Rect = augment.extend(this, {
				constructor: constructor,
				type: 'rect'
			});

			this.Text = augment.extend(this, {
				constructor: function(text) {
					constructor.call(this);
					this.properties.text = text;
				},
				type: 'text'
			});
		});

		var root = new RootNode();

		this.Shape = Shape;
		this.root = root;

		return this;
	};

	//Set up flags

	for (var flag in App.flags) {
		if (!App.flags.hasOwnProperty(flag)) continue;
		App.flags[flag].match = function(val) {
			return val.match(this.regex);
		};
	}

	//Properties set once on setup

	App.setup = {
		renderer: 'html',
		debounce: 100,
		ratio: 1,
		supportsCanvas: false,
		supportsSVG: false,
		lineWrapRatio: 0.9,
		renderers: ['html', 'canvas', 'svg']
	};

	App.dpr = function(val) {
		return val * App.setup.ratio;
	};

	//Properties modified during runtime

	App.vars = {
		preempted: false,
		resizableImages: [],
		debounceTimer: null,
		cache: {}
	};

	//Pre-flight

	(function() {
		var devicePixelRatio = 1,
			backingStoreRatio = 1;

		var canvas = newEl('canvas');
		var ctx = null;

		if (canvas.getContext) {
			if (canvas.toDataURL('image/png').indexOf('data:image/png') != -1) {
				App.setup.renderer = 'canvas';
				ctx = canvas.getContext('2d');
				App.setup.supportsCanvas = true;
			}
		}

		if (App.setup.supportsCanvas) {
			devicePixelRatio = global.devicePixelRatio || 1;
			backingStoreRatio = ctx.webkitBackingStorePixelRatio || ctx.mozBackingStorePixelRatio || ctx.msBackingStorePixelRatio || ctx.oBackingStorePixelRatio || ctx.backingStorePixelRatio || 1;
		}

		App.setup.ratio = devicePixelRatio / backingStoreRatio;

		if (!!document.createElementNS && !!document.createElementNS(SVG_NS, 'svg').createSVGRect) {
			App.setup.renderer = 'svg';
			App.setup.supportsSVG = true;
		}
	})();

	//Exposing to environment and setting up listeners
	register(Holder, 'Holder', global);

	if (global.onDomReady) {
		global.onDomReady(function() {
			if (!App.vars.preempted) {
				Holder.run();
			}
			if (global.addEventListener) {
				global.addEventListener('resize', resizeEvent, false);
				global.addEventListener('orientationchange', resizeEvent, false);
			} else {
				global.attachEvent('onresize', resizeEvent);
			}

			if (typeof global.Turbolinks == 'object') {
				global.document.addEventListener('page:change', function() {
					Holder.run();
				});
			}
		});
	}

})(function(fn, name, global) {
	var isAMD = (typeof define === 'function' && define.amd);
	var isNode = (typeof exports === 'object');
	var isWeb = !isNode;

	if (isAMD) {
		define(fn);
	} else {
		//todo: npm/browserify registration
		global[name] = fn;
	}
}, this);