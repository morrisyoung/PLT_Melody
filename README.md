This is the course project of COMS-W4115 Programming Language and Translator at Columbia.

It is contributed by Tong Ge, Jingsi Li and Shuo Yang.

We designed a music language called Melody, and used the OCaml to build a translator for it.

To compile the scanner.mll, ast.ml, parser.mly, compile.ml and melody.ml, simply run "make -f Makefile" in the Unix-like system.

(For contributors: If you find some intermediate files generated (such as *.cmi, *.cmo), please add them to the .gitignore file such that they will not be pushed into the online repository.)

**************************************************************************************************************
What I learn from this project:

From this project experience, I feel that “writing a good software” is not that easy. When we have finished something, we will find that there are still some more interesting and challenging things which should also be done. But if there is no enough time, the only thing left is pity. So the first thing I learned is that, no matter what we are dealing with, we should start as early as possible. Enough time will give us the opportunity to product a better work, not just a finished work although it can also work. To tell the truth, we have some delays during the project process, and we have experienced a very tough month in the last 30 days, handing in only a finished software. I admit that we have truly gained a lot from this project, for both language compiler itself and the team developing. But it is really a pity, at least for me, that I have not enough time to make a better work. By now, I have a thorough understanding about the programming language and compiler, and I know how to make our language better (for Music only because we just designed a music language). But I just don’t have time to make it. I’m sorry about this.

It’s my fault to make our whole project in a single file (compile.ml). I think this kind of thing should be avoided in software engineering, especially for the team developing. Putting nearly everything in a single file means that all of us should work on that single file, which further means that there is huge possibility that we will tough others’ codes when we solve the problem of ourselves, bringing more bugs. I have experienced this in our development, and I definitely believe that an appropriate developing style can enable us to avoid this kind of problem. We should keep different codes with different functions in different modules, and keep their interface between the modules as consistent as possible. When someone is responsible for one specific module, that person should guarantee that he can use the input from last module appropriately, and generate the right output format for the following module. By doing this, we can keep the wasting time of the team as less as possible. And for each member itself, he should have the ability to separately handle the problem from the module himself. We can make whole bunch of things more easily work in order by doing this.

Actually, it is not a good thing to start programming right after you get an idea. A whole project is not a programming game. We should schedule the structure of our project first. We should get a general idea about what features we should implement, otherwise in the future, when we want to implement a new idea, maybe we should go through the whole project, from scanner to compiler in the present case. By having a full idea first, we can avoid working on 500 lines codes in whole 20 days, revising after revising. A good start is half done, and we will never forget that.

Programming is very interesting, and exciting. I cannot describe the happiness after I write several effective codes, or find out some bugs in my program. During the final’s week, I just gave about 4 days to the other 3 courses’ review, and the other time is all spent on this programming project, even during my sleeping. I don’t care so much about the scores of my courses. I think I should do the right thing as a 23 young man, a future’s computer engineer. Unfortunately, I am not a clever problem-solver, at least by now. But it doesn’t matter. I believe I will become one in the future.

At last, I thank my teammates, Tong Ge and Jingsi Li, very much. You are all good programmers. You also taught me a lot about programming, and teamwork. Thank you for accompanying me for a whole term at Columbia. I hope we can all be better in the near future!


