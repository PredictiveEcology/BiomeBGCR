#include <Rcpp.h>
#include "../src/Biome-BGC/src/include/bgc.h"
#include "../src/Biome-BGC/src/include/pointbgc.h"

using namespace Rcpp;

//' Execute BGC from R
//'
//' @param `iniFiles` is the path to the input `ini` files
//'
//' @return an integer error code (0 means no error)
// [[Rcpp::export]]
int bgcExecute(CharacterVector argv, StringVector iniFiles) {

	std::vector<std::string> stdargv;
	stdargv.push_back("bcg");

	// argv contains all the param options without the ini file
    std::string _argv = Rcpp::as<std::string>(argv);
    size_t offset = std::string::npos;
	do
	{
		offset = _argv.find_first_of(" ");
		std::string sub = _argv.substr(0, offset);
		stdargv.push_back(sub);
		_argv = _argv.substr(offset + 1, std::string::npos);
	}
	while (offset != std::string::npos);

	// prepare the first parameters to be passed except the ini file
	char** argvFinal = new char*[stdargv.size() + 1];
	for (int i = 0; i < stdargv.size(); i++)
	{
		std::string& str = stdargv[i];
		argvFinal[i] = new char[str.size() + 1];
		strcpy(argvFinal[i], str.c_str());
	}

	int res = EXIT_SUCCESS;

	for (int i = 0; i < iniFiles.size(); i++)
	{
		std::string file = Rcpp::as<std::string>(iniFiles[i]);
		// allocate and fill in the last parameter (ini file)
		argvFinal[stdargv.size()] = new char[file.size() + 1];
		strcpy(argvFinal[stdargv.size()], file.c_str());

		int res = execute(argvFinal, stdargv.size() + 1);

		delete[] argvFinal[stdargv.size()];

		if (res != EXIT_SUCCESS)
		{
			// deallocate the argv struct
			for (int i = 0; i <stdargv.size(); i++)
				delete[] argvFinal[i];

			delete[] argvFinal;

			return res;
		}
	}

	// deallocate the argv struct
	for (int i = 0; i <stdargv.size(); i++)
		delete[] argvFinal[i];

	delete[] argvFinal;

  	return 0;
}
