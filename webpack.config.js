'use strict';

const path = require('path');
const webpack = require('webpack');
const ScreepsWebpackPlugin = require('./build/screepsWebpackPlugin');


module.exports = {
	devtool: 'source-map',

    entry: './src/index.js',

    output: {
        path: path.resolve(__dirname, 'dist'),
        filename: 'main'
    },

    module: {
        rules: [
            {
                test: /\.purs$/,
                use: [
                    {
                        loader: 'purs-loader',
                        options: {
                            src: [
                                'src/**/*.purs'
                            ],
                            spago: true,
                            pscIde: true
                        }
                    }
                ]
            }
        ]
    },

    resolve: {
        modules: [ 'node_modules' ],
        extensions: [ '.purs', '.js' ]
    },

    plugins: [
        new webpack.LoaderOptionsPlugin({
          debug: true
        }),
        new ScreepsWebpackPlugin()
    ]
}
