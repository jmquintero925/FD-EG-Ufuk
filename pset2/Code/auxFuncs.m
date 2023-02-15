% File wirh auxiliary functions for the
classdef    auxFuncs
    methods     ( Static = true )
        % Solve BGP
        function [out,flag]  = solveBGP(par)
            % Set optim options
            mopts       = optimset('Display','iter','MaxFunEvals',500,'MaxIter',200,...
                                   'TolX',1.0e-8,'TolFun',1.0e-10);
            % Define Objective function
            obj_Fun     = @(x) auxFuncs.eqSolve(x,par);
            % Solve for the fixed point
            [w,fval,flag] = fzero(obj_Fun,[0.1,1],mopts);
            % If algorithm converged
            if(flag==1)
                % Calculate the eqilibrium
                [~,out] = obj_Fun(w);
                % Calculate growth
                out.g = log(par.lambda)*(out.Fn(1)*(out.xn+out.xe(1))+sum(out.Fn(2:end).*out.xl));
                out.fval = fval;
            end
            flag = (flag==1);
        end
        % Solve the inner loops
        function [error,out] = eqSolve(x,par)
            if(x<=0|| x>1)
                error = 10;
                return;
            end
            % Equilibrium objects
            out.w = x;
            % Solve value function
            [out,flag] = auxFuncs.valueFuncIt(par,out);
            % Check if value function was solved
            if(~flag)
                error = 10;
                return;
            end
            % Solve the firm size distribution
            [out,flag] = auxFuncs.solveFirmDist(par,out);
            % Check if firm size distribution was solved
            if(~flag)
                error = 10;
                return;
            end
            % Solve for the new wage 
            w  = auxFuncs.solveWage(par,out);
            % Check for error
            error = w-x;

        end
        % Function that iterates the value function
        function [out,flag] = valueFuncIt(par,out)
            % Index for states
            m = (1:par.m)';
            % Profits for leader
            pi = ((1-par.tau)*(1-par.lambda.^(-m)));
            % Begin guessing value functions
            vl = pi/par.rho;
            vf = zeros(par.m,1);
            vn = 0;
            V    = [vl;vn;vf];
            % Some extra parmeters
            error   = 10;
            tol     = 1e-08;
            flag    = true;
            Delta   = 1/50;
            counter = 0;
            % Begin loop
            while(error>tol)
                counter = counter +1;
                % Unpack Value from previous iterations
                vl = V(m);
                vn = V(par.m+1);
                vf = V((m+par.m+1));
                Vf = [vn;vf];
                % Calculate innovation rates
                xl = ((diff(vl))/((1-par.s)*par.alpha*out.w)).^(1/(par.gamma-1));
                xl = [xl;0];
                xf = ((par.phi*vn +(1-par.phi)*vf(1:end-1)-vf(2:end))/((1-par.s)*par.alpha*out.w)).^(1/(par.gamma-1));
                xf = [xf;0];
                xn = ((vl(1)-vn)/((1-par.s)*par.alpha*out.w)).^(1/(par.gamma-1));
                xe = ((par.phi*vn +(1-par.phi)*vf)/(par.alphaT*out.w)).^(1/(par.gamma-1));
                xne= (vl(1)/(par.alphaT*out.w)).^(1/(par.gamma-1));
                % Calculate right hand side 
                Al = pi - (1-par.s)*par.alpha*out.w*(1/par.gamma).*xl.^par.gamma + ... 
                    (par.phi*xf + par.delta + par.phi*xe).*(vn-vl) - ...
                    (1-par.phi)*(xf +xe).*diff([vn;vl]) + xl.*[diff(vl);0];
                %----------------------------------------------------------
                Af = -(1-par.s)*par.alpha*out.w*(1/par.gamma).*xf.^par.gamma + ... 
                        xf.*(par.phi*vn +(1-par.phi)*Vf(1:end-1)-vf) + ...
                        par.delta*(vn-vf) - xl.*[diff(vf);0] - xe.*vf; 
                %----------------------------------------------------------
                An = -(1-par.s)*par.alpha*out.w*(1/par.gamma).*xn.^par.gamma + ... 
                      xn.*(vf(1)+vl(1)-2*vn) + xne*(0.5*vf(1)-vn);
                %----------------------------------------------------------
                % Update value functions               
                A    = [Al;An;Af];
                Vnew = V - (par.rho*V - A)*Delta;
                % Calculate the new error term
                error = max(abs(V-Vnew));
                if(counter>1e5)
                   flag = false;
                   break;
                end
                % Update value function 
                V = Vnew;
            end
            % Save objects
            out.xl = xl;
            out.xf = xf;
            out.xe = [xne;xe];
            out.xn = xn;
            out.V  = Vnew;



        end
        % Function that solves the firm size distribution
        function [eq,flag] = solveFirmDist(par,eq)
            % Begin with a guess for the firm size dist
            M       = ones(par.m,1)/(par.m+1);
            m0      = 1/(par.m+1);
            idx     = (2:(par.m-1))';
            idxf    = idx+1;
            idxb    = idx-1;
            % Unpack entrants
            xe      = eq.xe(2:end); 
            % some variables for looping
            error   = 10;
            tol     = 1e-08;
            flag    = true;
            Delta   = 1/50;
            counter = 0;
            % Begin loop
            while(error>tol)
                % Update counter
                counter = counter +1;
                % Update the derivative
                A = M(idxb).*eq.xl(idxb) + ... 
                    M(idxf).*((1-par.phi)*(eq.xf(idxf)+xe(idxf))) - ...
                    M(idx).*(eq.xl(idx)+eq.xf(idx)+par.delta+xe(idx));
                % Now deal with corners
                A1 = m0*(2*eq.xn + eq.xe(1)) + M(2)*((1-par.phi)*(eq.xf(2)+xe(2))) - ...
                     M(1)*(eq.xl(1)+eq.xf(1)+par.delta+xe(1));
                Am = M(end-1).*eq.xl(end-1) - M(end)*(eq.xf(end)+par.delta+xe(end));
                % Finally update the distribution
                Mnew = M + Delta*[A1;A;Am];
                % Get error term
                error = max(abs(M-Mnew));
                % Check counter
                if(counter>5e3)
                   flag = false;
                   break;
                end
                % Update firm size distribution
                M   = Mnew;
                m0  = 1-sum(M);
            end
            % Save results
            eq.Fn = [m0;M];

        end
        % Function that solves for the new wage
        function out = solveWage(par,eq)
            m = (0:par.m)';
            M = eq.Fn;
            % Solve for the left hand side
            lhs = sum(M(2:end).*(par.alpha*(eq.xf.^par.gamma+eq.xl.^par.gamma)+par.alphaT*eq.xe(2:end).^par.gamma));
            lhs = 1- lhs/par.gamma - M(1)*(2*par.alpha*eq.xn^par.gamma + par.alphaT*eq.xe(1)^par.gamma)/par.gamma;
            % Solve for the right hand side
            rhs = sum(M./(par.lambda.^m));
            % Solve for the wage
            out = rhs/lhs;

        end
    end
end